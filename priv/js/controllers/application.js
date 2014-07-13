/*!
 * @overview  Collective - Introspection UI for distributed Erlang systems
 * @copyright Copyright (c) 2014, Alexander Koeck
 * @license   Licensed under BSD license
 *            See https://raw.github.com/lx7/collective/master/LICENSE
 */

Collective.ApplicationController = Ember.Controller.extend({
    needs: ["node"],
    alert: undefined,
    init: function() {
    },
    actions: {
        closeAlert: function() {
            this.set('alert', undefined);
        },
        addNode: function() {
            $('#add-node-modal').foundation('reveal', 'close');
            Collective.Socket.send(["connect", this.get("newNodeName"), this.get("newNodeCookie")]);
        }
    }
});
