PRIVDIR := ../priv

all : $(PRIVDIR)/gpio_driver.so

override CFLAGS += -fpic -shared

$(PRIVDIR)/gpio_driver.so : gpio_driver.o
	$(CC) -shared -fpic $(LDFLAGS) -o $@ $^

lean:
	$(RM) -f $(PRIVDIR)/gpio_driver.so gpio_driver.o
