window.onload= function() {
    var canvas = document.getElementById("bintree");
    var context = canvas.getContext("2d");

};
function drawTreeNode(canvas, context, node){
    var r = 10;
    var x = 300;
    var y = 100;
    drawEllipse(canvas, context, "red", x, y, r);
    drawText(canvas, context, x, y, r, node.key);
}
function drawTree(canvas, context, tree){
    var node = tree.root;
    var children = [];
    var level = 0;
    
    if(node != null){
        node.place = {x:300, y:100, r:10};
    }
    children.push(node);
}
function treeSearch(node, key){
    if(node == null || node.key == key){
        return node;
    }
    if(key < node.key){
        return treeSearch(node.left, key);
    }else{
        return treeSearch(node.right, key);
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
function treeInsert(tree, key){
    var node={key:key,left:null,right:null,parent:null};
    var x = tree.root;
    var y = x;
    while(x != null){
        y = x;
        if(x.key < key){
            x = x.left;
        }else{
            x = x.right;
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
function treeDelete(tree, node){
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
    context.font = "bold 1em sans-serif";
    context.fillText(value, x, y+r/2);
}
