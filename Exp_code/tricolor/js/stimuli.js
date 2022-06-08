// Generating stimuli
// based on expt.default
//      .n
//      .type
//      [.type]
//

function genStimuli(config){
    var stimuli = [];
    // generate / load stimulus data.
    for(var i=0; i<config.n; i++){
        var newCircle = jQuery.extend(true, {}, config.circle);
        newCircle.center = genPosition(config.environment.center, config.environment.radius, config.circle.radius);
        newCircle.color = hsl2hex((config.colors[i%config.colors.length]+config.coffset)%1, 0.5, 0.5);
        stimuli.push(newCircle);
    }
    return(stimuli);
}

function genPosition(center, r_bound, r_obj){
    position = {   x: (Math.random()-0.5)*2, 
                    y: (Math.random()-0.5)*2};
    while(xyNorm(xyDiff(position, center)) > (r_bound - r_obj)){
        position = {   x: (Math.random()-0.5)*2, 
                        y: (Math.random()-0.5)*2};
    }
    return(position);
}