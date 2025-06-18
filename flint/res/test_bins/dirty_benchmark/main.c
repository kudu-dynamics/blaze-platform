#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/uaccess.h>
#include <linux/cdev.h>

#define DEVICE_NAME "vulnerable_dev"
#define BUFFER_SIZE 32  // Small buffer to demonstrate overflow

static char buffer[BUFFER_SIZE];
static int overflow_int = 2147483647; // Max value for signed int

static int dev_open(struct inode *inodep, struct file *filep) {
    printk(KERN_INFO "vulnerable_dev: Device opened\n");
    return 0;
}

static ssize_t dev_read(struct file *filep, char *user_buffer, size_t len, loff_t *offset) {
    return simple_read_from_buffer(user_buffer, len, offset, buffer, BUFFER_SIZE);
}

// BUFFER OVERFLOW: User can write more than BUFFER_SIZE bytes
static ssize_t dev_write(struct file *filep, const char *user_buffer, size_t len, loff_t *offset) {
    printk(KERN_INFO "vulnerable_dev: Writing %zu bytes\n", len);

    if (len > BUFFER_SIZE) {
        printk(KERN_WARNING "vulnerable_dev: Potential buffer overflow!\n");
    }

    // Dangerous: No bounds checking!
    copy_from_user(buffer, user_buffer, len);
    return len;
}

// INTEGER OVERFLOW: Simple integer overflow when read
static long dev_ioctl(struct file *filep, unsigned int cmd, unsigned long arg) {
    printk(KERN_INFO "vulnerable_dev: Integer before overflow = %d\n", overflow_int);
    overflow_int += arg;  // Overflow occurs if arg is large enough
    printk(KERN_INFO "vulnerable_dev: Integer after addition = %d\n", overflow_int);
    return 0;
}

static int dev_release(struct inode *inodep, struct file *filep) {
    printk(KERN_INFO "vulnerable_dev: Device closed\n");
    return 0;
}

static struct file_operations fops = {
    .open = dev_open,
    .read = dev_read,
    .write = dev_write,
    .unlocked_ioctl = dev_ioctl,
    .release = dev_release,
};

static int major_number;
static struct cdev cdev;

static int __init vuln_init(void) {
    dev_t dev;

    if (alloc_chrdev_region(&dev, 0, 1, DEVICE_NAME) < 0) {
        printk(KERN_ALERT "vulnerable_dev: Failed to allocate major number\n");
        return -1;
    }

    major_number = MAJOR(dev);
    cdev_init(&cdev, &fops);
    if (cdev_add(&cdev, dev, 1) < 0) {
        printk(KERN_ALERT "vulnerable_dev: Failed to add cdev\n");
        unregister_chrdev_region(dev, 1);
        return -1;
    }

    printk(KERN_INFO "vulnerable_dev: Loaded with major number %d\n", major_number);
    return 0;
}

static void __exit vuln_exit(void) {
    dev_t dev = MKDEV(major_number, 0);
    cdev_del(&cdev);
    unregister_chrdev_region(dev, 1);
    printk(KERN_INFO "vulnerable_dev: Unloaded\n");
}

module_init(vuln_init);
module_exit(vuln_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Example Author");
MODULE_DESCRIPTION("Linux Kernel Module with Buffer Overflow and Integer Overflow");
MODULE_VERSION("1.0");
