/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

/** Set up Ember routing */
Collective.Router.map(function() {
    this.resource('nodes');
    this.resource('node', { path: '/node/:node_id' }, function() {
        this.route('graph');
        this.route('processes');
    });
});

/** 
 * Ember Route for /index 
 * Transition to nodes
 */
Collective.IndexRoute = Ember.Route.extend({
    redirect: function() {
        this.transitionTo('nodes');
    }
});

/** 
 * Ember Route for /nodes 
 * Overview of all connected Erlang nodes
 */
Collective.NodesRoute = Ember.Route.extend({
    model: function() {
        return Collective.Node.list();
    }
});

/** 
 * Ember Route for /node/[id] 
 * Provide node model and redirect to nodes on failure
 */
Collective.NodeRoute = Ember.Route.extend({
    model: function(params) {
        return Collective.Node.load(params.node_id);
    },
    afterModel: function(model) {
        if (!model) {
            this.transitionTo('nodes')
        }
    },
    setupController: function(controller, model) {
        controller.set('model', model);
    }
});

/** 
 * Ember Route for /node/[id]/index 
 * Transition to node graph
 */
Collective.NodeIndexRoute = Ember.Route.extend({
    redirect: function() {
        this.transitionTo('node.graph');
    }
});

/** 
 * Ember Route for /node/[id]/graph 
 * Show process structure graph for a given node
 */
Collective.NodeGraphRoute = Ember.Route.extend({
    model: function(params) {
        return this.modelFor('node');
    }
});

/** 
 * Ember Route for /node/[id]/processes 
 * List processes for a given node
 */
Collective.NodeProcessesRoute = Ember.Route.extend({
    model: function(params) {
        return this.modelFor('node').get('processes');
    }
});

/** 
 * Ember Application Route 
 * Provides basic application environment
 */
Collective.ApplicationRoute = Ember.Route.extend({
    activate: function() {                
        var self = this;                                                                                                   
        Collective.Socket.connect("stream");
       
        Collective.Socket.register('node', function(msg) {
            if (msg.delete) {
                Collective.Node.remove(msg.id);
            } else {
                Collective.Node.add(msg.data);
            }
        });
        Collective.Socket.register('processes', function(msg) {
            var node = Collective.Node.load(msg.data[0].node);
            if (node) {
                node.setProcesses(msg.data);
            }
        });
        Collective.Socket.register('error', function(msg) {
            console.log("Socket error: " + msg.message);
            self.controllerFor('application').set('alert', msg.message);
        });
    },
    model: function() {
        return {
            nodes: Collective.Node.list()
        }
    },
    actions: {
        error: function(reason, transition) {
            this.transitionTo('nodes');
        }
    }
});

