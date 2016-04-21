//very pseudo js code, with annotations linking back to Sean's R analyze_tat.R script

//make sure `_weightIndex` is ticking up correctly

if ((_weightIndex + 1) <= _weightVector.length && firstRounded == false){ //in R: if (n <= 5) { #If we haven't moved past the end of the vector
     adjustedPrice = adjustedPrice;   //In R: psnap[i] <- tan(D[i]) #This is the price that will be offered to subjects in the following round, prior to being snapped to the grid
} else if (_weightIndex >= _weightVector.length) { //In R, the next few lines are psnap[i] <- (psnap[i-1]+(0.01*sign(Z[i]))) #End of vector has been reached

    if (priceDiff > 0.0) {
        adjustedPrice = (roundContext.price) - 0.01;  //removed rounding
    } else if (priceDiff < 0.0) {
        adjustedPrice = (roundContext.price) + 0.01;  //removed rounding
    } else {
        adjustedPrice = (roundContext.price);         //removed rounding, and chg adjustedPrice to roundContext.price
    }
}
if ((_weightIndex + 1) <= _weightVector.length){
    firstRounded = true; //now, going forward (after round 5), you'll always get kicked here    
}

// NOTE! that ifelse only works is addExcessDemand1  is implemented like: `_weightIndex = Math.min(_weightIndex + 1, _weightVector.length - 1); //_weightIndex never moves beyond end of _weightVector`
// see addExcessDemand1 or addExcessDemand2 for details. 

// snap prices to given price grid when: 
// config's `_snapPriceToGrid` is true
// and (_weightIndex + 1) <= _weightVector.length
if (_snapPriceToGrid == true){
    // From R:  if (pgrid==r) { #We won't snap this price or future rounds to the grid
    if (adjustedPrice == roundContext.price){
        _snapPriceToGrid = false;
        adjustedPrice = roundContext.price //it reads to me: basically, if adjustPrice converged, stop changing prices. 
    } else {
        // From R:   prindex <- which.min((pr-psnap[i])^2) #index of correct grid point
        //           pgrid <- pr[prindex] #This is the price snapped to the grid
        adjustedPrice = priceSnappedToGrid(adjustedPrice);
    }
}

// in Sean's R code, there is a block: 
//#  if (dataTat$Round[(i-1)*s+1]==1) { #Resets to grid snapping in the first round of each period
//#    snap <- 1
//#  }
// I am deliberately ignoring that, since turing on or off _snapPriceToGrid is a config file field. 
// it gets reset to 1 at the start of earch Period... I think by js.    