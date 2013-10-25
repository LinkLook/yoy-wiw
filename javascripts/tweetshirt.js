window.onload= function() {
    var button = document.getElementById("previewButton");
    button.onclick = previewHandler;
    var canvas = document.getElementById("tshirt");
    canvas.onclick = function (){
        window.location = canvas.toDataURL("image/png");
    };
};
function getSelectObjectValue(eleId){
    var obj = document.getElementById(eleId);
    var idx = obj.selectedIndex;
    return obj[idx].value;
}
function previewHandler(){
    var canvas = document.getElementById("tshirt");
    var context = canvas.getContext("2d");
    var bgcolor = getSelectObjectValue("bgColor");
    fillBgColor(canvas, context, bgcolor);

    var shape = getSelectObjectValue("shape");
    if (shape == "squares") {
        doTimes(canvas, context, drawSquare, 20, 0);
    } else if (shape == "triangle"){
        doTimes(canvas, context, drawPath, 4, 5);
    } else if (shape == "circles"){
        doTimes(canvas, context, drawCircle, 20, 0);
    } else {
        doTimes(canvas, context, drawCircle, 7, 0);
        doTimes(canvas, context, drawSquare, 7, 0);
        doTimes(canvas, context, drawPath, 2, 5);
    }
    drawText(canvas, context);
    drawBird(canvas, context);
    //context.fillRect(10, 10, 100, 100);
};
function doTimes(canvas, context, drawFunction, times, arg){
    for (var c=0; c<times; c++){
        drawFunction(canvas, context, arg);
    }
}

function randomPoint(canvas){
//    var x = Math.floor(Math.random()*canvas.width);
//    var y = Math.floor(Math.random()*canvas.height);
//    return JSON.parse("{\"x\":"+x+",\"y\":"+y+"}");
    var p = {};
    p.x = function(){
        return Math.floor(Math.random()*canvas.width);
    };
    p.y = function(){
        return Math.floor(Math.random()*canvas.height);
    };
    return p;
}
function drawCircle(canvas, context, arg){
    var radius = Math.floor(Math.random()*40)+10;
    context.beginPath();
    context.arc(randomPoint(canvas).x(), randomPoint(canvas).y(), radius, 0, 2 * Math.PI, true);
    context.fillStyle = "lightblue";
    context.fill();
}
function drawSquare(canvas, context, arg){
    var width = Math.floor(Math.random()*40)+10;
    context.fillStyle = "lightblue";
    context.fillRect(randomPoint(canvas).x(), randomPoint(canvas).y(), width, width);
}
function fillBgColor(canvas, context, bgcolor){
    context.fillStyle = bgcolor;
    context.fillRect(0, 0, canvas.width, canvas.height);
}
function randomArroud(x, max, deltaMin, deltaRange){
    var delta = Math.floor(Math.random()*deltaRange)+deltaMin;
    if (Math.random()*2 > 1) {
        delta = 0 - delta;
    }
    x = x+delta;
    if (x <= 0 || x >= max){
        x = x-delta-delta;
    }
    return x;
}
function randomPointArroud(canvas, pointA, pointB){
    var k = (pointB.y-pointA.y)/(pointB.x-pointA.x);
//    var b = pointB.y-k*pointB.x;
    var c = pointB.y+k*pointB.x;
    var pointC = {};

    pointC.x = randomArroud(pointB.x, canvas.width, 30, 20);

    var delta = Math.floor(Math.random()*20);
    if (Math.random()*2 > 1) {
        delta = 0 - delta;
    }
    pointC.y = c-k*pointC.x+delta;
    if (pointC.y <=0 || pointC.y >= canvas.height){
        pointC.y = c-k*pointC.x-delta;
    }
    return pointC;
}
function drawPath(canvas, context, lines){

    var pointA = {};
    var pointB = {};
    var pointC = {};

    context.beginPath();
    pointA.x = randomPoint(canvas).x();
    pointA.y = randomPoint(canvas).y();
    //Move to Point A.
    context.moveTo(pointA.x, pointA.y);

    pointB.x = randomArroud(pointA.x, canvas.width, 30, 20);
    pointB.y = randomArroud(pointA.y, canvas.height, 20, 15);
    //Line to Point B.
    context.lineTo(pointB.x, pointB.y);

    while (lines > 0){
        pointC = randomPointArroud(canvas, pointA, pointB);
        //Line to Point C.
        context.lineTo(pointC.x, pointC.y);
        pointA.x = pointB.x;
        pointA.y = pointB.y;
        pointB.x = pointC.x;
        pointB.y = pointC.y;
        lines = lines-1;
    }

    context.closePath();
    context.lineWith = 5;
    context.stroke();
    context.fillStyle = "red";
    context.fill();
}
function drawText(canvas, context){
    context.fillStyle = getSelectObjectValue("textColor");
    context.textAlign = "left";
    context.font = "italic 1em serif";
    context.fillText("I saw this tweet", 20, 40);

    context.font = "italic bold 1.2em Times, serif";
//    context.textBaseline = "middle";
    context.strokeText(getSelectObjectValue("tweets"), 30, 100);

    context.textAlign = "right";
    context.font = "bold 1em sans-serif";
    context.fillText("and all I got was this lousy t-shirt!",
                     canvas.width-20, canvas.height-40);
}
function drawBird(canvas, context){
    var bird = new Image();
    bird.src = "https://0.gravatar.com/avatar/66069cdf358d71bdec2dd2c7ad627dc5";
    bird.onload = function () {
        context.drawImage(bird, 20, 120, 70, 70);
    };
}
