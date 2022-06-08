
function Display(canvas){
    this.ctx = canvas.getContext("2d");
    this.color = '#FFFFFF';
    this.canvas = canvas;

    this.getParams = function(){
        // separate function to recalculate stuff in case window moves.
        this.width = this.canvas.width;
        this.height = this.canvas.height;
        this.scale = Math.min(this.width, this.height)/2;
        var rect = this.canvas.getBoundingClientRect();
        this.offset = {x: rect.left,
                       y: rect.top};
        this.center = {x: this.width/2, y: this.height/2};
    }

    this.clear = function(){
        this.ctx.fillStyle=this.color;
        this.ctx.fillRect(0,0,this.width,this.height);
    }

    this.draw = function(thing){
        switch(thing.type){
            case 'line':
                this.drawLine(thing);
                break;
            case 'circle':
                this.drawCircle(thing);
                break;
            case 'rectangle':
                this.drawRectangle(thing);
                break;
            case 'triangle':
                this.drawTriangle(thing);
                break;
            case 'image':
                this.drawImage(thing);
                break;
            case 'text':
                this.drawText(thing);
                break;
            default:
                console.log('Unidentified thing type: ' + thing.type);
                break;
        }
    }
	
	this.abs2rel = function(abs_xy, ref_xy = {x:0, y:0}){
		// convert into unscaled coordinates rel to center
        abs_xy = {  x: (abs_xy.x - this.center.x - this.offset.x)/this.scale,
                    y: (abs_xy.y - this.center.y - this.offset.y)/this.scale};
		rel_pos = {x: abs_xy.x - ref_xy.x,
                   y: abs_xy.y - ref_xy.y};
        rel_pos.distance = xyNorm(rel_pos);
        rel_pos.angle = Math.atan2(rel_pos.y, rel_pos.x)/Math.PI*180;
        return(rel_pos);
	}

    this.drawLine = function(line){
        // Takes line object{width, color, start{x,y}, end{x,y}}

        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.lineWidth=line.width;
        this.ctx.strokeStyle=line.color;
        this.ctx.moveTo(line.start.x*this.scale + this.center.x,line.start.y*this.scale + this.center.y);
        this.ctx.lineTo(line.end.x*this.scale + this.center.x,line.end.y*this.scale + this.center.y);
        this.ctx.stroke();

        this.ctx.restore();
    }

    this.drawCircle = function(circle){
        // circle = {center{x,y}, radius, color, border {width, color}}
        this.ctx.save();
        this.ctx.beginPath();
        this.ctx.arc(circle.center.x*this.scale + this.center.x, 
			circle.center.y*this.scale + this.center.y, 
			circle.radius*this.scale, 
			0, Math.PI*2, false);
        this.ctx.fillStyle=circle.color;
        this.ctx.fill();
        if(typeof(circle.border) !== 'undefined'){
            this.ctx.lineWidth = circle.border.width*this.scale;
            this.ctx.strokeStyle = circle.border.color;
            this.ctx.stroke();
        }

        this.ctx.restore();
    }

    this.drawRectangle = function(rectangle){   
        // rectangle = {center{x,y}, height, width, color, border {width, color}}
        this.ctx.save();
        this.ctx.translate(rectangle.center.x*this.scale + this.center.x, rectangle.center.y*this.scale + this.center.y);
        this.ctx.rotate(rectangle.angle*Math.PI/180);
        if(typeof(rectangle.border) !== 'undefined'){
            this.ctx.fillStyle=rectangle.border.color;
            this.ctx.fillRect((-rectangle.width/2-rectangle.border.width)*this.scale,
                (-rectangle.height/2-rectangle.border.width)*this.scale,
                (rectangle.width+rectangle.border.width*2)*this.scale,
                (rectangle.height+rectangle.border.width*2)*this.scale);
        }
        this.ctx.fillStyle=rectangle.color;
        this.ctx.fillRect(-rectangle.width/2*this.scale,
            -rectangle.height/2*this.scale,
            rectangle.width*this.scale,
            rectangle.height*this.scale);
        this.ctx.restore();
    }

    this.drawImage = function(image){
        // image center{x,y}, width, height, angle, image=new Image(); im.src=image.file;
        this.ctx.save();
        this.ctx.translate(image.center.x*this.scale + this.center.x, image.center.y*this.scale + this.center.y);
        this.ctx.rotate(image.angle*Math.PI/180);
        this.ctx.drawImage(image.image,
            -image.width/2*this.scale,
            -image.height/2*this.scale,
            image.width*this.scale,
            image.height*this.scale);
        this.ctx.restore();
    }

    this.drawTriangle = function(triangle){  
        //triangle = {color, center {x,y}, angle, width, height}
        this.ctx.save();
        this.ctx.translate(triangle.center.x*this.scale + this.center.x, triangle.center.y*this.scale + this.center.y);
        this.ctx.rotate(triangle.angle*Math.PI/180);

        if(typeof(triangle.border) !== 'undefined'){
            this.ctx.beginPath();
            radians = Math.atan2(triangle.height, triangle.width/2)/2;
            dh = Math.sin(radians)*triangle.border.width;
            dw = Math.cos(radians)*triangle.border.width;
            this.ctx.moveTo((-triangle.width/2-dw)*this.scale,(-triangle.height/2-dh)*this.scale);
            this.ctx.lineTo((triangle.width/2+dw)*this.scale,(-triangle.height/2-dh)*this.scale);
            this.ctx.lineTo(0,(triangle.height/2+triangle.border.width)*this.scale);
            this.ctx.closePath();

            this.ctx.fillStyle=triangle.border.color;
            this.ctx.fill(); 
        }
        this.ctx.beginPath();
        this.ctx.moveTo((-triangle.width/2)*this.scale,(-triangle.height/2)*this.scale);
        this.ctx.lineTo((triangle.width/2)*this.scale,(-triangle.height/2)*this.scale);
        this.ctx.lineTo(0,(triangle.height/2)*this.scale);
        this.ctx.closePath();

        this.ctx.fillStyle=triangle.color;
        this.ctx.fill(); 
        this.ctx.restore();
    }

    this.drawText = function(text){     
        this.ctx.fillStyle = text.color;
        this.ctx.font = text.style;
        this.ctx.fillText(text.words, text.x*this.scale + this.center.x, text.y*this.scale + this.center.y);
    }
	
	this.drawMondrianMask = function(){
        for(var i =0; i<100; i++){
            var minp = 1/Math.pow(i+1,0.9);
            var maxp = 1/Math.pow(i+1,0.5);
            this.draw({ type: 'rectangle',
                            center: {  x: (Math.random()-0.5)*2,
                                        y: (Math.random()-0.5)*2},
                            width: (Math.random()*(maxp-minp)+minp)*2,
                            height: (Math.random()*(maxp-minp)+minp)*2,
                            angle: Math.random()*180,
                            color: hsl2hex(Math.random(), 
                                            Math.random()*0.5+0.5, 
                                            Math.random()*0.5+0.5),
                            border: {   width: Math.random()*0.05+0.001,
                                        color: hsl2hex(Math.random(), 
                                                        Math.random()*0.5, 
                                                        Math.random()*0.5)}});
        }
	}
}

// check if in bounds
function xyDiff(a,b){
    return({x: a.x - b.x, y: a.y - b.y});
}

function xyNorm(a){
    return(Math.pow(Math.pow(a.x,2)+Math.pow(a.y,2),.5));
}

function xyInside(xy, thing){
    switch(thing.type){
        case 'line':
            return(false);
            break;
        case 'circle':
            distance = xyNorm(xyDiff(thing.center, xy));
            return(distance <= thing.radius);
            break;
        case 'rectangle':
            offset = xyDiff(xy,thing.center);
            return((Math.abs(offset.x) <= thing.width/2) & (Math.abs(offset.y) <= thing.height/2));
            break;
        case 'triangle':
            distance = xyNorm(xyDiff(thing.center, xy));
            return(distance <= Math.max(thing.width, thing.height)/2);
            break;
        case 'image':
            offset = xyDiff(xy,thing.center);
            return((Math.abs(offset.x) <= thing.width/2) & (Math.abs(offset.y) <= thing.height/2));
            break;
        case 'text':
            return(false);
            break;
        default:
            console.log('Unknown object type: ' + thing.type);
    }
}

function randomXYcircle(center, radius){
    position = { x: center.x + (Math.random()-0.5)*2*radius,
                 y: center.y + (Math.random()-0.5)*2*radius};
    while(xyNorm(xyDiff(position, center)) > radius){
        position = { x: center.x + (Math.random()-0.5)*2*radius,
                     y: center.y + (Math.random()-0.5)*2*radius};
    }
    return(position);
}



// Color helpers
function componentToHex(c) {
    var hex = c.toString(16);
    return hex.length == 1 ? "0" + hex : hex;
}
function hue2rgb(p, q, t){
    if(t < 0) t += 1;
    if(t > 1) t -= 1;
    if(t < 1/6) return p + (q - p) * 6 * t;
    if(t < 1/2) return q;
    if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
    return p;
}
function hsl2hex(h, s, l){
    var r, g, b;

    if(s == 0){
        r = g = b = l; // achromatic
    }else{
        var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
        var p = 2 * l - q;
        r = hue2rgb(p, q, h + 1/3);
        g = hue2rgb(p, q, h);
        b = hue2rgb(p, q, h - 1/3);
    }

    return "#" + componentToHex(Math.round(r*255)) + componentToHex(Math.round(g*255)) + componentToHex(Math.round(b*255));
}

function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
}

function sample(a) {
	return(a[Math.floor(Math.random() * a.length)]);
}

//generate random normal        
var zstore;
function rnorm(mu, sigma){
        var outpt;
        if (zstore){
                outpt = zstore*sigma+mu;
                zstore = null;
        } else {
                var r = Math.sqrt(-2 * Math.log(Math.random()));
                var t = 2 * Math.PI * Math.random();
                zstore = r * Math.cos(t);
                outpt = r * Math.sin(t) * sigma + mu;
        }
        return outpt;
}

function cornerSpot(i,n,r){
    return({x: 1-r-i*2*r, y: 1-r})
}


function debugLog(message){
    if(expt.debug){console.log(((new Date()) - trial.state.start)/1000 + ': ' + message)};
}


