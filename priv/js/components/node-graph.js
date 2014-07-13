/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

d3.Charts = d3.Charts || {};
d3.Charts.Node = function() {
    // Graph dimensions
    var width = $('body').width();
        height = $('body').height() - 45;
    
    // Graph data
    var nodes = [],
        nodes_idx = {},
        links = [],
        links_idx = {},
        monitors = [],
        monitors_idx = {};

    // SVG entities
    var svg,
        zoom,
        graphg,
        link_path,
        monitor_path,
        circle,
        text,
        tip;

    // Process node color definition
    var pcolors = [ "#222", "#333", "#444", "#555", "#666", "#777", "#888", 
                    "#999", "#aaa", "#bbb", "#ccc", "#ddd", "#eee", "#fff" ];
   
    /**
     * Draws a force directed graph for processes and apps
     * @param {Array} selection 
     * @return {Object} chart
     */ 
    function chart(selection) {
        selection.each(function(data) {
            // Reset graph in case the SVG element disappeared 
            // (SPA navigation etc.)
            if (!d3.select("#graph").select("svg")[0][0]) {
                reset();
            }

            // Set node data and rebuild index
            nodes = data.processes;
            rebuild_index();
           
            // Connect nodes
            enter_links();
            exit_links();

            // Initialize graph if necessary
            if (!svg) {
                init();
            }

            // Update graph with node and link data
            update();
                

            /** Rebuild the node index */
            function rebuild_index() {
                nodes_idx = {};
                nodes.forEach(function(node) {
                    nodes_idx[node.id] = node;
                });
            }

            /** Create link objects */
            function enter_links() {
                nodes.forEach(function(node) {
                    node.get('links').forEach(function(link) {
                        var a, b;
                        if (true || node < nodes_idx[link]) {
                            a = node; 
                            b = nodes_idx[link];
                        } else {
                            a = nodes_idx[link];
                            b = node;
                        }
                        if (a && b) {
                        var id = "L " + a.id + "-" + b.id; 
                            if (!links_idx[id]) {
                                var l = {
                                    id: id,
                                    source: a,
                                    target: b,
                                    type: 'link'
                                };
                                links.push(l);
                                links_idx[id] = l;
                            }
                        }
                    });
                    node.get('monitors').forEach(function(monitor) {
                        var a = node; 
                        var b = nodes_idx[monitor];
                        var id = "M " + a.id + "-" + b.id; 
                        if (!monitors_idx[id]) {
                            var l = {
                                id: id,
                                source: a,
                                target: b,
                                type: 'monitor'
                            };
                            monitors.push(l);
                            monitors_idx[id] = l;
                        }
                    });
                });
            }

            /** Remove old links */
            function exit_links() {
                for (var i = 0; i < links.length; i++) {
                    if (!links[i].source || !nodes_idx[links[i].source.id] ||
                        !links[i].target || !nodes_idx[links[i].target.id]) {
                        delete links_idx[links[i].id];
                        links.splice(i--, 1);
                    }
                }
                for (var i = 0; i < monitors.length; i++) {
                    if (!monitors[i].source || !nodes_idx[monitors[i].source.id] ||
                        !monitors[i].target || !nodes_idx[monitors[i].target.id]) {
                        delete monitors_idx[monitors[i].id];
                        monitors.splice(i--, 1);
                    }
                }
            }

            /** Initialize graph and force layout */
            function init() {
                force = d3.layout.force()
                  .size([width, height]);

                tip = d3.tip()
                    .attr('class', 'd3-tip')
                    .offset([-30, 0])
                    .html(function(d) {
                        return [
                            '<h2>', (d.name? d.name: d.id), '</h2>', 
                            '<div>Pid: ', d.id, '</div>', 
                            '<div>Initial Function: ', d.initial_fun, '</div>',
                            '<div>Current Function: ', d.current_fun, '</div>',
                            '<div>Memory: ', d.memory, '</div>',
                            '<div>Reductions: ', d.reds_delta, '</div>',
                            '<div>Message Queue: ', d.msgq, '</div>',
                            '<div>Trap Exit: ', d.trap_exit, '</div>',
                            '<div>Group Leader: ', d.group_leader, '</div>',
                        ].join('');
                    });

                zoom = d3.behavior.zoom()
                    .scaleExtent([0.3, 2])
                    .on("zoom", move);

                svg = d3.select("#graph").append("svg")
                    .attr("width", width)
                    .attr("height", height)
                    .call(tip);
                
                $(window).on("resize", resize);
                
                // Build arrow for monitor path
                svg.append("svg:defs").selectAll("marker")
                        .data(["pmarker-monitor"])     
                    .enter().append("svg:marker") 
                        .attr("id", String)
                        .attr("viewBox", "0 -5 10 10")
                        .attr("refX", 9)
                        .attr("refY", 0)
                        .attr("markerWidth", 6)
                        .attr("markerHeight", 6)
                        .attr("orient", "auto")
                    .append("svg:path")
                        .attr("d", "M0,-5L10,0L0,5");

                svg.append("rect")
                    .attr("class", "overlay")
                    .attr("x", 0)
                    .attr("y", 0)
                    .attr("width", width)
                    .attr("height", height)
                    .call(zoom);
               
                graphg = svg.append("g"); 
                link_path = graphg.append("g").selectAll("link_path");
                circle = graphg.append("g").selectAll("circle");
                monitor_path = graphg.append("g").selectAll("monitor_path");
                text = graphg.append("g").selectAll("text");
            }

            /** Update graph with node and link data */
            function update() {
                force.nodes(nodes)
                    .links(links)
                    .linkDistance(40)
                    .charge(-250)
                    .chargeDistance(1800)
                    .on("tick", tick);

                link_path = link_path.data(links, 
                    function(d) { return d.id; });
                link_path.enter().append("path")
                    .attr("class", function(d) { return "plink " + d.type; })
                    .attr("pointer-events", "none");
                link_path.exit().remove();
                
                monitor_path = monitor_path.data(monitors, 
                    function(d) { return d.id; });
                monitor_path.enter().append("path")
                    .attr("class", function(d) { return "plink " + d.type; })
                    .attr("pointer-events", "none")
                    .attr("marker-end", "url(#pmarker-monitor)");
                monitor_path.exit().remove();

                circle = circle.data(nodes, function(d) { return d.id; });
                circle.enter().append("circle")
                    .attr("class", function(d) { 
                        return "pprocess " 
                            + (d.trap_exit? "tx ": "") 
                            +  d.get('type')
                    })
                    .attr("r", function(d) { return memRadius(d.memory); })
                    .on('mouseover', tip.show)
                    .on('mouseout', tip.hide)
                    //.on("click", click)
                    .call(force.drag);
                circle.transition().duration(2000)
                    .style("fill", function(d) { 
                        return redsColor(d.get('reds_delta')); 
                    })
                    .attr("r", function(d) { return memRadius(d.memory); })
                circle.exit().remove();
                    
                text = text.data(nodes, function(d) { return d.id; });
                text.enter().append("text")
                    .attr("x", 8)
                    .attr("y", ".31em")
                    .attr("pointer-events", "none")
                    .text(function(d) { return d.name; });
                text    
                    .text(function(d) { return d.name? d.name: ""; });
                text.exit().remove();
                
                if (!circle.enter().empty() || !circle.exit().empty() ||
                    !link_path.enter().empty() || !link_path.exit().empty()) {
                    force.start();
                }
            }

            /** Reset graph */
            function reset() {
                nodes = [];
                nodes_idx = {};
                links = [];
                links_idx = {};
                monitors = [];
                monitors_idx = {};
                svg = undefined;
                graphg = undefined;
                link_path = undefined;
                monitorlink_path = undefined;
                circle = undefined;
                text = undefined;
            }

            /** Force animation tick */
            function tick() {
                link_path.attr("d", line);
                monitor_path.attr("d", arc);
                circle.attr("transform", transform);
                text.attr("transform", transform);
            }

            /** Update SVG dimensions on window resize */
            function resize() {
                width = $('body').width();
                height = $('body').height();

                svg.attr("width", width);
                svg.attr("height", height);
                
                force.size([width, height]).start();
            }

            /** Move and zoom */
            function move() {
                var t = d3.event.translate,
                    s = d3.event.scale;
                zoom.translate(t);
                graphg.attr("transform", "translate("+t+")scale("+s+")");
            }

            /** TODO: Toggle children on click */
            function click(d) {
                for (var i = 0; i < nodes.length; i++) {
                    if (nodes[i].group_leader == d.id && nodes[i].id != d.id) {
                        delete nodes_idx[nodes[i].id];
                        nodes.splice(i--, 1);
                    }
                }
                exit_links(); 
                update();
            }

            /** Create path segment */
            function line(d) {
                var dx = d.target.x - d.source.x,
                    dy = d.target.y - d.source.y,
                    dr = 0;
                return "M" + d.source.x + "," + d.source.y + 
                       "L" + d.target.x + "," + d.target.y;
            }
            
            /** Create elliptical arc path segments */
            function arc(d) {
                var dx = d.target.x - d.source.x,
                    dy = d.target.y - d.source.y,
                    dr = Math.sqrt(dx * dx + dy * dy);
                return "M" + d.source.x + "," + d.source.y + 
                       "A" + dr + "," + dr + " 0 0,1 " 
                           + d.target.x + "," + d.target.y;
            }

            /** Transform SVG element */
            function transform(d) {
                return "translate(" + d.x + "," + d.y + ")";
            }
            
            /** Calculate circle radius by process memory */
            function memRadius(mem) {
                return Math.round(Math.log(mem/20)*2);
            }

            /** Calculate circle color by process activity (reds) */
            function redsColor(reds) {
                var i = Math.round(Math.log(Math.max(reds,1)));
                return pcolors[i];
            }
        
        });
    }

    return chart;
};


Collective.NodeGraphComponent = Ember.Component.extend({
    classNames: 'node-graph',

    chart: d3.Charts.Node(),

    draw: function() {
        var self = this;
        d3.select(this.get('element'))
            .data([ this.get('data') ])
            .call(this.get('chart'));
    }.observes('data.processes').on('didInsertElement'),
});

