var display = null;
var interval = null;
var countdown = null;
var client = parseClient();
var trial = {}
var trial = {
    trial_n: -1,
    sid: client.sid,
    expt: expt.name,
    phase: 'start',
    error_threshold: expt.error_threshold
};
if(expt.debug){
    expt.timing.presentation = 2000;
    expt.timing.mask = 300;
}

// initialize event handlers.
// start consent phase.

$(document).ready(nextPhase);
// nextPhase();

function nextPhase(){
    switch(trial.phase){
        case 'start':
            if(client.type == 'preview'){
                $('#startButton')[0].innerHTML = "You must ACCEPT the HIT before you can start.";
            } else {
                trial.phase = 'start';
                $('#startButton')[0].disabled = false;
                $('#startButton')[0].innerHTML = "Click here to start!";
                trial.phase = 'consent';
                display = new Display(document.getElementById('myCanvas'));
                $(document).on('keypress', onKeyPress);
                $('#myCanvas').on("click", onMouseDown);
                $('#myCanvas').on("mousemove", onMouseMove);
            }
            $("#trial")[0].style.display = 'none';
            $("#instructions")[0].style.display = 'none';
            $("#consent")[0].style.display = "block";
            
            break;
        case 'consent':
            $("#trial")[0].style.display = 'none';
            $("#consent")[0].style.display = "none";
            $("#instructions")[0].style.display = "block";
            trial.phase = 'instructions';
            break;
        case 'instructions':
            trial.phase = 'loading';
            $("#trial")[0].style.display = 'none';
            $("#instructions")[0].style.display = "none";
            display.clear();
            newTrial();
            break;
        case 'loading':
            break;
        case 'pre-trial':
            trial.phase = 'presentation';
            showStimuli();
            break;
        case 'presentation':
            break;
        case 'mask':
            break;
        case 'response':
            if(trial.state.selected == null & trial.stimuli.length == trial.state.nReported){
                completeTrial();
            }
            break;
        case 'feedback':
            if(trial.trial_n < (expt.maxTrials-1)){
                // start new trial
                trial.phase = 'loading';
                $("#trial")[0].style.display = 'none';
                $("#instructions")[0].style.display = "none";
                display.clear();
                newTrial();
            } else {    
                // calculate bonus.
                client.bonus = Math.round(client.score / expt.maxTrials /2)/100;
                // display something about thanks for participating, this is your bonus.
                $('#bonusText')[0].innerHTML='$'+client.bonus.toFixed(2);
                $("#trial")[0].style.display = 'none';
                $("#consent")[0].style.display = "none";
                $("#instructions")[0].style.display = "none";
                $("#done")[0].style.display = "block";
                // on click: submit
                trial.phase = 'finished';
            }
            break;
        case 'finished':
            submitExternal(client);
            break;
        default:
            break;
    }
}

function genTrial(nObjects){
    trial.stimuli = [];
    // generate / load stimulus data.
    for(var i=0; i<nObjects; i++){
        position = randomXYcircle({x:0, y:0}, (expt.default.environment.radius - expt.default.circle.radius));
        var newCircle = jQuery.extend(true, {}, expt.default.circle);
        newCircle.center = position;
        newCircle.color = hsl2hex((expt.default.colors[i%expt.default.colors.length]+expt.default.coffset)%1, 0.5, 0.5);
        trial.stimuli.push(newCircle);
    }
    trial.expt = 'generated';
    trial.seed = 0;
    trial.chain = 0;
    trial.iter = trial.trial_n;
}

function startTrial(){
    $('#loading')[0].style.display = 'none';
    trial.settings = expt;


    $("#trial")[0].style.display = 'inline';
    display.getParams();
    display.clear();
    display.draw(expt.default.environment);
    $('#subtitle')[0].innerHTML='Press <em>Enter/Return</em> to start.';
    trial.phase = 'pre-trial';
}

function newTrial(){
    trial.trial_n++;
    trial.phase = 'loading';
    updateScoreText();
    trial.responses = [];
    trial.state = {nReported: 0,
                   selected: null,
                   start: new Date(),
                   reporting: null,
                   displayTime: Math.round(expt.timing.presentation/1000)};
    if(trial.trial_n < expt.default.practice.length){
        genTrial(expt.default.practice[trial.trial_n]);
        trial.expt = 'practice';
        startTrial();
    } else {
        readServer({sid: trial.sid, expt: expt.name, trial_n: trial.trial_n});
    }

    // Get list of elements (ideally from server)
    // genTrial(expt.default.n);
    // start trial
}

function showStimuli(){
    $('#myCanvas')[0].style.cursor="none";
    $("#instructions")[0].style.display = 'none';
    $("#trial")[0].style.display = 'inline';
    display.clear();
    display.draw(expt.default.environment);
    trial.stimuli.forEach(display.draw, display);
    $('#subtitle')[0].innerHTML='Memorize this display. ' + trial.state.displayTime +' seconds left.';
    setTimeout(showMask, expt.timing.presentation);
    countdown = window.setInterval(displayTimer, 1000);
}

function showMask(){
    trial.phase = 'mask';
    display.clear();
    display.drawMondrianMask();
    $('#subtitle')[0].innerHTML='';
    setTimeout(getResponses, expt.timing.mask);
}

// report objects
function redraw(){
    if(trial.phase == 'response'){
        display.clear();
        display.draw(expt.default.environment);
        trial.responses.forEach(display.draw, display);
    } else {
        clearInterval(interval);
    }
}

function displayTimer(){
    trial.state.displayTime--;
    if(trial.state.displayTime > 0){
        $('#subtitle')[0].innerHTML='Memorize this display. ' + trial.state.displayTime +' seconds left.';
    } else {
        clearInterval(countdown);
    }
}

function getResponses(){
    display.clear();
    display.draw(expt.default.environment);
    trial.phase = 'response';

    for(i =0; i < trial.stimuli.length; i++){
    	var newCircle = jQuery.extend(true, {}, expt.default.circle);
        newCircle.center = cornerSpot(i, trial.stimuli.length, expt.default.circle.radius+expt.default.circle.border.width);
        newCircle.color = trial.stimuli[i].color;
        trial.responses.push(newCircle);
    }
    trial.state.nReported = trial.responses.map(function(obj){return(xyInside(obj.center, expt.default.environment))}).reduce((a, b) => a + b, 0);


    $('#subtitle')[0].innerHTML='Click to select an object, then click to place. <br> ' + (trial.stimuli.length-trial.state.nReported) + ' objects left.<br>Press <em>Enter/Return</em> when done.';
;
    $('#myCanvas')[0].style.cursor="default";

    if(expt.debug){
        $("#debugControls")[0].style.display = 'inline';
    }

    interval = window.setInterval(redraw, 30);
}

function fillResponses(type){
    if(expt.debug && trial.phase=='response'){
        switch(type){
            case 'uniform':
                trial.responses.forEach((resp)=>{resp.center=randomXYcircle({x:0, y:0}, (expt.default.environment.radius - expt.default.circle.radius))});
                break;
            case 'center':
                trial.responses.forEach((resp)=>{resp.center={x:0, y:0}});
                break;
            case 'perfect':
                for(var i=0; i<trial.responses.length; i++){
                    trial.responses[i].center = trial.stimuli[i].center;
                }
                break;
            default:
                break;
        }
        trial.state.nReported = trial.responses.map(function(obj){return(xyInside(obj.center, expt.default.environment))}).reduce((a, b) => a + b, 0);
        $('#subtitle')[0].innerHTML='Click to select an object, then click to place. <br> ' + (trial.stimuli.length-trial.state.nReported) + ' objects left. <br>Press <em>Enter/Return</em> when done.';
    }
}

// trial data -> JSON -> post.
function completeTrial(){
    clearInterval(interval);
    if(trial.phase == 'response'){
        // this works if ignoring color....
        var lines = [];
        // trial.rmse = 0;
        trial.error = 0;
        if(expt.error_match == 'blind'){
            m = new Munkres();
            D = []
            for(var i = 0; i < trial.stimuli.length; i++){
                D.push(trial.responses.map((resp)=>xyNorm(xyDiff(resp.center, trial.stimuli[i].center))));
            }
            var index = m.compute(D);
            // add line for each link
            for(var i = 0; i < index.length; i++){
                lines.push({    type:"line",
                                color:'#ff0000',
                                width: 2,
                                start: trial.stimuli[index[i][0]].center,
                                end: trial.responses[index[i][1]].center
                    });
                trial.error += Math.abs(D[index[i][0]][index[i][1]]);
            }
        }
        if(expt.error_match == 'color'){
            // procedure for color-specific matching.
            colors = trial.stimuli.map((x)=>(x.color)).unique();
            for(var j = 0; j < colors.length; j++){
                tmps = trial.stimuli.filter((x)=>(x.color == colors[j]));
                tmpr = trial.responses.filter((x)=>(x.color == colors[j]));
                m = new Munkres();
                D = []
                for(var i = 0; i < tmps.length; i++){
                    D.push(tmpr.map((resp)=>xyNorm(xyDiff(resp.center, tmps[i].center))));
                }
                var index = m.compute(D);
                // add line for each link
                for(var i = 0; i < index.length; i++){
                    lines.push({    type:"line",
                                    color:'#ff0000',
                                    width: 2,
                                    start: tmps[index[i][0]].center,
                                    end: tmpr[index[i][1]].center
                        });
                    // trial.rmse += Math.pow(D[index[i][0]][index[i][1]],2);
                    trial.error += Math.abs(D[index[i][0]][index[i][1]]);

                }
            }
        }


        // trial.rmse = Math.pow(trial.rmse/trial.stimuli.length, 1/2)/2;
        trial.error = (trial.error/trial.stimuli.length);

        // update score
        // trial.score = Math.round((1-trial.rmse/0.57)*100);
        trial.score = Math.round(Math.max(0, Math.min(1, (0.4-trial.error)/0.4))*100);
        client.score += trial.score;
        trial.phase = 'feedback';

        // post data.
        writeServer();

        // show feedback display
        display.clear();
        display.draw(expt.default.environment);
        lines.forEach(display.draw, display);
        trial.stimuli.forEach(display.draw, display);
        trial.responses.forEach(display.draw, display);

        $('#subtitle')[0].innerHTML='Your score for this trial is: <strong>' + trial.score + '</strong><br>Press <em>"Enter/Return"</em> to continue.';

        updateScoreText();        
    }
}

function updateScoreText(){
    $('#score')[0].innerHTML='Trial <strong>' + (trial.trial_n+1) + '/' + expt.maxTrials + '</strong>.    Total Score: <strong>' + client.score + '</strong>';
}

function onKeyPress(event){
    // check state of trial
    if(event.which==13){
        nextPhase();
    }
}

function onMouseDown(event){
    // check state of trial
    debugLog('mousedown');
    if(trial.phase == 'response'){
        // check if click is in canvas
        if((event.target || event.srcElement).id == 'myCanvas'){
            var xy = display.abs2rel({x: event.clientX, y: event.clientY});

            // check current state:
            // is an object selected?
            // if not, did we just select an object?
            // if not, did we just place an object?
            if(trial.state.selected == null){
                debugLog('mousedown -> none selected');

                clickInside = trial.responses.map(function(obj){return(xyInside(xy, obj))});
                if(clickInside.reduce((a, b) => a + b, 0) > 0){
                    debugLog('mousedown -> new selected');

                    trial.state.selected = clickInside.findIndex((x)=>x);
                    trial.responses[trial.state.selected].border.width = 0.02;
                    trial.responses[trial.state.selected].border.color = trial.responses[trial.state.selected].color;
                    trial.state.reporting = 'xy';
                } else {
                    // debugLog('mousedown -> new placed');

                    // if(trial.state.nReported < trial.stimuli.length){
                    //     var newCircle = jQuery.extend(true, {}, expt.default.circle);
                    //     newCircle.center = xy;
                    //     newCircle.color = '#999999';
                    //     trial.responses.push(newCircle);
                    //     trial.state.nReported++;
                    //     $('#subtitle')[0].innerHTML='Click to place an object. Objects left: ' + (trial.settings.nObjects - trial.state.nReported);
                    //     if(trial.state.nReported==trial.stimuli.length){
                    //         $('#subtitle')[0].innerHTML='Click on objects to move them as needed. <br>Press <em>"Enter/Return"</em> to continue.';
                    //     }
                    // }
                }
            } else {
                debugLog('mousedown -> something already selected');

                trial.responses[trial.state.selected].center = xy;
                trial.responses[trial.state.selected].border.width = 0.005;
                trial.responses[trial.state.selected].border.color = '#000000';
                trial.state.selected = null;
                trial.state.reporting = null;
                trial.state.nReported = trial.responses.map(function(obj){return(xyInside(obj.center, expt.default.environment))}).reduce((a, b) => a + b, 0);
			    $('#subtitle')[0].innerHTML='Click to select an object, then click to place. <br> ' + (trial.stimuli.length-trial.state.nReported) + ' objects left. <br>Press <em>Enter/Return</em> when done.';
            }
        }
    }
}


function onMouseMove(event){
    if(trial.phase == 'response' & trial.state.selected != null){
        // only bother with all this if we are in response phase and have something selected.
        switch(trial.state.reporting){
            case 'xy':
                trial.responses[trial.state.selected].center = display.abs2rel({x: event.clientX, y: event.clientY});
                break;
            case 'angle':
                relMouse = display.abs2rel({x: event.clientX, y: event.clientY}, trial.responses[trial.state.selected].center);
                trial.responses[trial.state.selected].angle = relMouse.angle;
                break;
        }
    }
}

Array.prototype.unique = function() {
    var arr = [];
    for(var i = 0; i < this.length; i++) {
        if(arr.indexOf(this[i]) == -1) {
            arr.push(this[i]);
        }
    }
    return arr;
}


// variant with clicking, placing, and dragging.
// function onMouseDown(event){
//     // check state of trial
//     debugLog('mousedown');
//     if(trial.phase == 'response'){
//         // check if click is in canvas
//         if((event.target || event.srcElement).id == 'myCanvas' &
//             display.abs2rel({x: event.clientX, y: event.clientY}).distance < expt.default.environment.radius){
//             var xy = display.abs2rel({x: event.clientX, y: event.clientY});

//             // check current state:
//             // is an object selected?
//             // if not, did we just select an object?
//             // if not, did we just place an object?
//             if(trial.state.selected == null){
//                 debugLog('mousedown -> none selected');

//                 clickInside = trial.responses.map(function(obj){return(xyInside(xy, obj))});
//                 if(clickInside.reduce((a, b) => a + b, 0) > 0){
//                     debugLog('mousedown -> new selected');

//                     trial.state.selected = clickInside.findIndex((x)=>x);
//                     trial.responses[trial.state.selected].border.color = '#990000';
//                     trial.state.reporting = 'xy';
//                 } else {
//                     debugLog('mousedown -> new placed');

//                     if(trial.state.nReported < trial.settings.nObjects){
//                         var newCircle = jQuery.extend(true, {}, expt.default.circle);
//                         newCircle.center = xy;
//                         newCircle.color = '#999999';
//                         trial.responses.push(newCircle);
//                         trial.state.nReported++;
//                         $('#subtitle')[0].innerHTML='Click to place an object. Objects left: ' + (trial.settings.nObjects - trial.state.nReported);
//                         if(trial.state.nReported==trial.settings.nObjects){
//                             $('#subtitle')[0].innerHTML='Click on objects to move them as needed. <br>Press <em>"Enter/Return"</em> to continue.';
//                         }
//                     }
//                 }
//             } else {
//                 debugLog('mousedown -> something already selected');

//                 trial.responses[trial.state.selected].center = xy;
//                 trial.responses[trial.state.selected].border.color = '#000000';
//                 trial.state.selected = null;
//                 trial.state.reporting = null;
//             }
//         }
//     }
// }


