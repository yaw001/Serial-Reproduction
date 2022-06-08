// experiment settings
var expt = {
	name: 'iterated_prototype',
	maxTrials: 10,
    debug: true,
    rmse_threshold: 0.5,
    rmse_match: 'color',
    readURL: 'http://experiments.evullab.org/iterated_prototype/request.php',
    saveURL: 'http://experiments.evullab.org/iterated_prototype/submit.php',
    timing: {
    	consent: null,
    	instructions: null,
    	presentation: 1000,
    	mask: 500,
    	response: null,
    	feedback: null
    },
    default: {
    	n: 15, 	
        practice: [3],
    	colors: [0, 0.333, 0.666],
    	coffset: 0.317,
    	type: 'circle',
    	environment: {
    		type: 'circle',
    		center: {x: 0, y:0},
    		radius: 0.95,
    		color: '#FFFFFF',
    		border: {width: 0.01, color: '#888888'}
    	},
    	circle: {
            type: 'circle',
            center: {   x: null, 
                        y: null},
            radius: 0.03,
            color: hsl2hex(0, 0, 0.2),
            border: {width: 0.005, color: '#000000'}
    	},
    response: {	
    		xy: true,
    		angle: false,			// make this work
    		color: false,			// make this work
    		size: false,			// make this work
    		aspect_ratio: false		// make this work
    	}
    }
};


