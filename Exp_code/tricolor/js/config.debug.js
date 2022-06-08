// experiment settings
var expt = {
	name: 'live',
	maxTrials: 10,
    debug: false,
    error_threshold: 0.4,
    error_match: 'color',
    readURL: 'request.php',
    saveURL: 'submit.php',
    timing: {
    	consent: null,
    	instructions: null,
    	presentation: 10000*1.5,
    	mask: 1000,
    	response: null,
    	feedback: null
    },
    default: {
    	n: 15,
        practice: [3, 9],
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


