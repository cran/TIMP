"examineFit" <-
function (resultfitModel, opt=vector()) 
{
     if(length(opt) == 0)
	plotoptions <-    resultfitModel$toPlotter$plotoptions
     else 
	plotoptions <- opt

     plotter(resultfitModel$toPlotter$model, 
     resultfitModel$toPlotter$multimodel, 
     resultfitModel$toPlotter$multitheta,
     plotoptions)

}
