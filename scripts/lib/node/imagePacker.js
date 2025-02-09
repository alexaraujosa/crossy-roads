/**
 * Bin Packaging Problem solver using a Binary Tree.
 * 
 * The size of the container for the bins is dynamic, expanding as needed.
 * 
 * Most of the time, all sorters (except random) produce the same output, however, if too much empty space is left empty within
 * the container, try other sorters.
 *
 * @class GrowingPacker
 */
export class GrowingPacker {
    constructor () {}

    fit(blocks) {
        let len = blocks.length;
        let w = len > 0 ? blocks[0].w : 0;
        let h = len > 0 ? blocks[0].h : 0;

        this.root = { x: 0, y: 0, w: w, h: h };

        for (let n = 0; n < len ; n++) {
            let block = blocks[n];
            let node = this.findNode(this.root, block.w, block.h)
            
            if (node) block.fit = this.splitNode(node, block.w, block.h);
            else block.fit = this.growNode(block.w, block.h);
        }
    }

    findNode(root, w, h) {
        if (root.used) return this.findNode(root.right, w, h) || this.findNode(root.down, w, h);
        else if ((w <= root.w) && (h <= root.h)) return root;
        else return null;
    }

    splitNode(node, w, h) {
        node.used = true;
        node.down  = { x: node.x, y: node.y + h, w: node.w, h: node.h - h };
        node.right = { x: node.x + w, y: node.y, w: node.w - w, h: h };
        
        return node;
    }

    growNode(w, h) {
        let canGrowDown  = (w <= this.root.w);
        let canGrowRight = (h <= this.root.h);
    
        let shouldGrowRight = canGrowRight && (this.root.h >= (this.root.w + w));
        let shouldGrowDown  = canGrowDown  && (this.root.w >= (this.root.h + h));
    
        if (shouldGrowRight) return this.growRight(w, h);
        else if (shouldGrowDown) return this.growDown(w, h);
        else if (canGrowRight) return this.growRight(w, h);
        else if (canGrowDown) return this.growDown(w, h);
        else return null;
    }

    growRight(w, h) {
        this.root = {
            used: true,
            x: 0,
            y: 0,
            w: this.root.w + w,
            h: this.root.h,
            down: this.root,
            right: { x: this.root.w, y: 0, w: w, h: this.root.h }
        };

        let node = this.findNode(this.root, w, h)
        if (node) return this.splitNode(node, w, h);
        else return null;
    }

    growDown(w, h) {
        this.root = {
            used: true,
            x: 0,
            y: 0,
            w: this.root.w,
            h: this.root.h + h,
            down:  { x: 0, y: this.root.h, w: this.root.w, h: h },
            right: this.root
        };

        let node = this.findNode(this.root, w, h)
        if (node) return this.splitNode(node, w, h);
        else return null;
    }

	static sorters = {
		random  : function (a, b) { return Math.random() - 0.5; },
		w       : function (a, b) { return b.w - a.w; },
		h       : function (a, b) { return b.h - a.h; },
		a       : function (a, b) { return b.area - a.area; },
		max     : function (a, b) { return Math.max(b.w, b.h) - Math.max(a.w, a.h); },
		min     : function (a, b) { return Math.min(b.w, b.h) - Math.min(a.w, a.h); },
	
		height  : function (a, b) { return GrowingPacker.sorters.msort(a, b, ['h', 'w']);               },
		width   : function (a, b) { return GrowingPacker.sorters.msort(a, b, ['w', 'h']);               },
		area    : function (a, b) { return GrowingPacker.sorters.msort(a, b, ['a', 'h', 'w']);          },
		maxside : function (a, b) { return GrowingPacker.sorters.msort(a, b, ['max', 'min', 'h', 'w']); },
	
		msort: function(a, b, criteria) {
			let diff;

			for (let n = 0 ; n < criteria.length ; n++) {
				diff = GrowingPacker.sorters[criteria[n]](a,b);
	
				if (diff != 0) return diff;  
			}
	
			return 0;
		},
	
		/**
		 * @param {"none"|"width"|"height"|"maxside"|"area"|"random"} sort 
		 * @param {*} blocks 
		 */
		now: function(sort, blocks) {
			if (sort != 'none') blocks.sort(GrowingPacker.sorters[sort]);
		}
	}
	
}