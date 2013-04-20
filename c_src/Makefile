PRIVDIR := ../priv

all : $(PRIVDIR)/gpio_drv.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/gpio_drv.so : gpio_drv.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

lean:
	$(RM) -f $(PRIVDIR)/gpio_drv.so gpio_drv.o
