
Collective.Node = Ember.Object.extend({
    id:             "",
    release:        0,
    smp:            false,
    cpus:           0,
    schedulers:     0,
    threads:        0,
    architecture:   "",
    processes:      [],
    apps:           [],
    _process_idx:   {},

    init: function() {
        this.set('processes', Ember.A());
        this._process_idx = {};
    },
    addProcess: function(data) {
        var record = this._process_idx[data.id];
        if (record) {
            for (var key in data) {
                if (data.hasOwnProperty(key)) {
                    record.set(key, data[key]);
                }
            }
        } else {
            record = Collective.Process.create(data);
            this.get('processes').pushObject(record);
            this._process_idx[data.id] = record;
        }
        return record;
    },
    setProcesses: function(data) {
        var self = this;
        var processes = Ember.A();
        var _process_idx = {};

        data.forEach(function(item) {
            var record = self._process_idx[item.id];
            if (record) {
                for (var key in item) {
                    if (item.hasOwnProperty(key)) {
                        record.set(key, item[key]);
                    }
                }
                record.compute();
            } else {
                record = Collective.Process.create(item);
            }
            processes.pushObject(record);
            _process_idx[item.id] = record;
        });

        this._process_idx = _process_idx;
        this.set('processes', processes);
    },
});

Collective.Node.reopenClass({
    list: function() {
        if (!this._list) {
            this._list = Ember.A();
        }
        return this._list;
    },
    load: function(id) {
        var res = this.list().findBy('id', id);
        if (res) {
            return res;
        } else {
            return undefined;
        }
    },
    add: function(data) {
        var record = this.load(data.id);
        if (record) {
            for (var key in data) {
                if (data.hasOwnProperty(key)) {
                    record.set(key, data[key]);
                }
            }
        } else {
            record = Collective.Node.create(data);
            this.list().pushObject(record);
        }
        return record;
    },
    remove: function(id) {
        var record = Collective.Node.load(id);
        if (record) {
            Collective.Node.list().removeObject(record);
        }
    }
});


