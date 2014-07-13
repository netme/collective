/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.NodeGraphController = Ember.ObjectController.extend({
    showLinks: true,
    showMonitors: true,
    chartData: function() {
        var self = this;
        return {
            name: "node-graph",
            processes: this.get('processes'),
            apps: this.get('apps'),
            showLinks: this.get('showLinks'),
            showMonitors: this.get('showMonitors')
        };
    }.property('processes.@each.reds', 'showLinks', 'showMonitors') 
});
