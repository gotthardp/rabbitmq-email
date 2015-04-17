include ../umbrella.mk

samples: test/data/samples.zip

test/data/samples.zip:
	wget http://www.hunnysoft.com/mime/samples/samples.zip -O $@
	unzip $@ -d $(@D)
