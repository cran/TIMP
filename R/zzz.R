".onLoad" <- function (lib, pack)
{

	init.dat()
	init.theta()
	init.kin()
	init.spec()	
	init.fc()
	init.het()
	init.res()
	init.fit()
	init.multitheta()
	init.multimodel()
	init.opt()
	init.kinopt()
	init.specopt()
	setGenerics()
	setMethods() 

	library.dynam(pack, pack, lib)

}
