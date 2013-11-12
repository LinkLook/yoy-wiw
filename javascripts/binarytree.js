var global_tree = null;
window.onload= function() {
    var canvas = document.getElementById("bintree");
    var context = canvas.getContext("2d");
    var makeTreeButton = document.getElementById("makeTree");
    makeTreeButton.onclick = function(){
        makeTreeHandler(canvas, context);
    };
    //    drawTreePlainText(canvas, context, tree);
    var deleteNodeButton = document.getElementById("deleteNode");
    deleteNodeButton.onclick = function(){
        deleteTreeNodeHandler(canvas, context);
    };
    makeTreeHandler(canvas, context);
};
function makeTreeHandler(canvas, context){
    var tree = {root:null, nodes:[]};
    var node = null;
    var key = 0;

    for(var i=0; i<10; i++){
        key = Math.floor(Math.random()*99);
        node = treeSearch(tree, key);
        if(node == null){
            node = {key:key,left:null,right:null,parent:null}; 
            treeInsertNode(tree, node);
            tree.nodes.push(node);
        }else{
            i--;
        }
    }
    global_tree = tree;
    drawTree(canvas, context, tree);
}
function deleteTreeNodeHandler(canvas, context){
    var nodeKeyInput = document.getElementById("nodeKey");
    var nodeKey = nodeKeyInput.value;
    var tree = global_tree;

    var node = treeSearch(tree, nodeKey);

    if(node == null){
        alert("Not find:key="+nodeKey+".");
    }else{
        treeDeleteNode(tree, node);
        drawTree(canvas, context, tree);
    }
}
function drawTreeNode(canvas, context, node){
    var face = node.face;
    if(face == undefined || face == null){
        alert("node face is undefined");
        return -1;
    }
    drawEllipse(canvas, context, node.face.color, face.x, face.y, face.r);
    drawText(canvas, context, face.x, face.y, face.r, node.key);
    if(node.parent != null){
        context.beginPath();
        context.lineWith = 5;
        context.moveTo(node.face.x, node.face.y);
        context.lineTo(node.parent.face.x, node.parent.face.y);
        context.stroke();
    }
    return 0;
}
function drawTreePlainTextNode(canvas, context, node, x, y){
    drawText(canvas, context, x, y, 10, node.key);        
    if(node.left != null){
        x = x+30;
        drawTreePlainTextNode(canvas, context, node.left, x, y);
    }
    if(node.right != null){
        x = x+30;
        drawTreePlainTextNode(canvas, context, node.right, x, y);
    }
}
function drawTreePlainText(canvas, context, tree){
    drawTreePlainTextNode(canvas, context, tree.root, 10, 300);
    for(var i=0; i<tree.nodes.length; i++){
        drawText(canvas, context, i*60+10, 350, 10, tree.nodes[i].key);
        var node = tree.nodes[i];
        if(node.parent != null){
            drawText(canvas, context, i*60+30, 350, 10, node.parent.key);
        }else{
            //        alert("Left:"+tree.nodes[i].left+"Right:"+tree.nodes[i].right);
        }
    }
}
function treeHeight(tree){
    var node = null;
    var h = 0;
    var m = 1;
    var children = [];

    if(treeEmpty(tree)){
        return 0;
    }
    children.push(tree.root);
    while(children.length > 0){
        if(m > 0){
            node = children.shift();
            if(node.left != null){
                children.push(node.left);
            }
            if(node.right != null){
                children.push(node.right);
            }
            m = m-1;
        }else{
            m = children.length;
            h = h+1;
        }
    }
    return h;
}
function treeEmpty(tree){
    return tree.root == undefined || tree.root == null;
}
function drawTree(canvas, context, tree){
    var node = null;
    var children = [];
    var m = 1;

    context.fillStyle = "white";
    context.fillRect(0, 0, canvas.width, canvas.height);

    if(treeEmpty(tree)){
        return 0;
    }

    setTreeRootNodeFace(canvas, tree.root);
    children.push(tree.root);
    while(children.length > 0){
        if(m > 0){
            node = children.shift();
            drawTreeNode(canvas, context, node);
            if(node.left != null){
                setTreeNodeFace(canvas, node.left, node, true);
                children.push(node.left);
            }
            if(node.right != null){
                setTreeNodeFace(canvas, node.right, node, false);
                children.push(node.right);
            }
            m = m-1;
        }else{
            m = children.length;
        }
    }
    return 0;
}
function setTreeRootNodeFace(canvas, rootNode){
    rootNode.face = {x:canvas.width/2, y:30, r:10, color:"lightblue"};
}
function setTreeNodeFace(canvas, node, parent, isLeft){
    var r = parent.face.r-1;
    var d = Math.sqrt(r*r/2);
    var hi = 4*(r-d)+20;    
    var wi = 0;
    var h = -1;
    var n = node;
    var x = 0;
    var y = parent.face.y + hi;

    while(n != null){
        h = h+1;
        n = n.parent;
    }
    wi = canvas.width/(Math.pow(2, h)+1);

    for(x=0; x<canvas.width; x=x+wi){
        if(x > parent.face.x){
            break;
        }
    }
    if(isLeft){
        x = x - wi;
    }
    node.face = {x:x, y:y, r:r, color:"lightblue"};
}

function treeSearch(tree, key){
    return treeSearchNode(tree.root, key);
}
function treeSearchNode(node, key){
    if(node == null || node.key == key){
        return node;
    }
    if(key < node.key){
        return treeSearchNode(node.left, key);
    }else{
        return treeSearchNode(node.right, key);
    }
}
function treeMinimum(node){
    if(node == null){
        return null;
    }
    while(node.left != null){
        node = node.left;
    }
    return node;
}
function treeMaximum(node){
    if(node == null){
        return null;
    }
    while(node.right != null){
        node = node.right;
    }
    return node;
}

function treeSuccessor(node){
    if(node.right != null){
        return treeMinimum(node.right);
    }
    var p = node.parent;
    while(p != null && node == p.right){
        node = p;
        p = p.parent;
    }
    return p;
}
function treePredecessor(node){
    if(node.left != null){
        return treeMaximum(node.left);
    }
    var p = node.parent;
    while(p != null && node == p.left){
        node = p;
        p = p.parent;
    }
    return p;
}
function treeInsertNode(tree, node){
    var key = node.key;
    var x = tree.root;
    var y = x;
    while(x != null){
        y = x;
        if(x.key < key){
            x = x.right;
        }else{
            x = x.left;
        }
    }
    node.parent = y;
    if(y == null){
        tree.root = node;
    }else if(y.key < key){
        y.right = node;
    }else{
        y.left = node;
    }
}
function treeChangeNode(tree, node, toNode){
    var p = node.parent;

    if(toNode != null){
        toNode.parent = p;
    }
    if(p != null){
        if(p.left == node){
            p.left = toNode;
        }else{
            p.right = toNode;
        }
    }else{
        tree.root = toNode;
    }
}
function treeDeleteNode(tree, node){
    if(node.left == null){
        treeChangeNode(tree, node, node.right);
        node.parent = null;
        node.right = null;
        return node;
    }
    if(node.right == null){
        treeChangeNode(tree, node, node.left);
        node.parent = null;
        node.left = null;
        return node;
    }
    var s = treeMinimum(node.right);
    treeChangeNode(tree, s, s.right);
    s.left = node.left;
    s.right = node.right;
    node.left.parent = s;
    node.right.parent = s;
    treeChangeNode(tree, node, s);
    return node;
}

function drawEllipse(canvas, context, color, x, y, r)
{
    var d = Math.sqrt(r*r/2);
    context.beginPath();
    context.arc(x+d, y, r, -Math.PI/4, Math.PI/4, false);
    context.arc(x, y-d, 2*r, Math.PI/4, 3*Math.PI/4, false);
    context.arc(x-d, y, r, 3*Math.PI/4, 5*Math.PI/4, false);
    context.arc(x, y+d, 2*r, 5*Math.PI/4, 7*Math.PI/4, false);
    context.closePath();
    context.lineWith = 5;
    context.stroke();
    context.fillStyle = color;
    context.fill();
}
function drawText(canvas, context, x, y, r, value){
    context.fillStyle = "black";
    context.textAlign = "center";
    context.font = "bold "+(Math.floor(r/2)+1)/5+"em serif";
    context.fillText(value, x, y+r/2);
}
