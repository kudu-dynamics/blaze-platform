import ghidra.program.model.pcode.*;
import ghidra.program.model.address.Address;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Java-side helpers that collapse multiple property accesses on a single
 * Ghidra object into one JNI round-trip, returning packed primitive arrays.
 *
 * Called from Haskell via Java.callStatic "PcodeHelper" "methodName" ...
 */
public class PcodeHelper {

    // -- Per-varnode fields (9 longs) --
    private static final int VN_FIELDS = 9;

    private static void packVarNode(ArrayList<Long> buf, VarnodeAST v) {
        if (v == null) {
            // Null varnode — pack zeros for all 9 fields
            for (int i = 0; i < VN_FIELDS; i++) buf.add(0L);
            return;
        }
        buf.add((long) v.getSize());
        buf.add(v.isConstant() ? 1L : 0L);

        Address addr = v.getAddress();
        if (addr != null) {
            buf.add(addr.getOffset());
            buf.add((long) addr.getAddressSpace().getSpaceID());
        } else {
            buf.add(0L); buf.add(0L);
        }

        Address pcAddr = v.getPCAddress();
        if (pcAddr != null && !pcAddr.equals(Address.NO_ADDRESS)) {
            buf.add(1L);
            buf.add(pcAddr.getOffset());
            buf.add((long) pcAddr.getAddressSpace().getSpaceID());
        } else {
            buf.add(0L); buf.add(0L); buf.add(0L);
        }

        HighVariable hv = v.getHigh();
        if (hv != null) {
            buf.add(1L);
            buf.add((long) hv.hashCode());
        } else {
            buf.add(0L); buf.add(0L);
        }
    }

    /**
     * Extract ALL pcode op + varnode data for a basic block in one JNI call.
     *
     * Returns a flat long[] with the structure:
     *   [numOps, for each op:
     *     seqTargetOffset, seqTargetSpaceId,   // 2: instruction address
     *     opcodeInt,                            // 1: PcodeOp.getOpcode()
     *     hasOutput (0 or 1),                   // 1
     *     if hasOutput: varnode basics [9]      //    (size, isConst, addrOff, addrSpId,
     *                                           //     hasPcAddr, pcOff, pcSpId, hasHigh, highHash)
     *     numInputs,                            // 1
     *     for each input: varnode basics [9]    // 9 each
     *   ]
     *
     * Replaces iterator traversal + per-op getMnemonic/getOutput/getInputs +
     * per-varnode extractVarNodeBasics (~9 JNI calls per op) with 1 call per block.
     */
    public static long[] extractBlockData(PcodeBlockBasic block) {
        ArrayList<Long> buf = new ArrayList<>();

        // Count ops first by iterating (we'll re-iterate for data)
        int numOps = 0;
        Iterator<PcodeOp> countIter = block.getIterator();
        while (countIter.hasNext()) { countIter.next(); numOps++; }

        buf.add((long) numOps);

        Iterator<PcodeOp> iter = block.getIterator();
        while (iter.hasNext()) {
            PcodeOpAST op = (PcodeOpAST) iter.next();

            // Sequence number target address
            SequenceNumber seqnum = op.getSeqnum();
            if (seqnum != null) {
                Address target = seqnum.getTarget();
                if (target != null) {
                    buf.add(target.getOffset());
                    buf.add((long) target.getAddressSpace().getSpaceID());
                } else {
                    buf.add(0L); buf.add(0L);
                }
            } else {
                buf.add(0L); buf.add(0L);
            }

            // Opcode as int (avoids String allocation from getMnemonic)
            buf.add((long) op.getOpcode());

            // Output varnode
            Varnode output = op.getOutput();
            if (output != null) {
                buf.add(1L);
                packVarNode(buf, (VarnodeAST) output);
            } else {
                buf.add(0L);
            }

            // Input varnodes
            Varnode[] inputs = op.getInputs();
            if (inputs != null) {
                buf.add((long) inputs.length);
                for (Varnode input : inputs) {
                    packVarNode(buf, (VarnodeAST) input);
                }
            } else {
                buf.add(0L);
            }
        }

        // Convert ArrayList<Long> to long[]
        long[] result = new long[buf.size()];
        for (int i = 0; i < buf.size(); i++) {
            result[i] = buf.get(i);
        }
        return result;
    }

    /**
     * Get the HighVariable for a specific varnode within a block.
     * Used for HighVariable cache misses after extractBlockData.
     *
     * @param block  the pcode block
     * @param opIndex  0-based index of the op within the block
     * @param vnIndex  -1 for output, 0..N for input index
     * @return the HighVariable, or null if the varnode has no HighVariable
     */
    public static HighVariable getBlockVarNodeHigh(
            PcodeBlockBasic block, int opIndex, int vnIndex) {
        Iterator<PcodeOp> iter = block.getIterator();
        PcodeOpAST op = null;
        for (int i = 0; i <= opIndex; i++) {
            op = (PcodeOpAST) iter.next();
        }
        if (op == null) return null;
        VarnodeAST vn;
        if (vnIndex < 0) {
            vn = (VarnodeAST) op.getOutput();
        } else {
            Varnode[] inputs = op.getInputs();
            if (inputs == null || vnIndex >= inputs.length) return null;
            vn = (VarnodeAST) inputs[vnIndex];
        }
        return vn != null ? vn.getHigh() : null;
    }

    // -- Legacy per-varnode helper (still used by mkHighVarNodeCached) --

    /**
     * Extract basic properties of a VarnodeAST in one JNI call.
     * Returns long[9] — same layout as packVarNode.
     */
    public static long[] extractVarNodeBasics(VarnodeAST v) {
        long[] r = new long[VN_FIELDS];
        r[0] = v.getSize();
        r[1] = v.isConstant() ? 1 : 0;

        Address addr = v.getAddress();
        r[2] = addr.getOffset();
        r[3] = addr.getAddressSpace().getSpaceID();

        Address pcAddr = v.getPCAddress();
        if (pcAddr != null && !pcAddr.equals(Address.NO_ADDRESS)) {
            r[4] = 1;
            r[5] = pcAddr.getOffset();
            r[6] = pcAddr.getAddressSpace().getSpaceID();
        }

        HighVariable hv = v.getHigh();
        if (hv != null) {
            r[7] = 1;
            r[8] = hv.hashCode();
        }

        return r;
    }

    /**
     * Extract the instruction address from a PcodeOp's sequence number.
     * Returns long[2]: [offset, spaceId].
     */
    public static long[] extractOpAddress(PcodeOp op) {
        Address target = op.getSeqnum().getTarget();
        long[] r = new long[2];
        r[0] = target.getOffset();
        r[1] = target.getAddressSpace().getSpaceID();
        return r;
    }
}
