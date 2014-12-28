var column_names = ['Name', 'Busy', 'Op', 'Vj', 'Vk', 'Qj', 'Qk'];


function heading(row, text) {
    var th = document.createElement('th');
    var text = document.createTextNode(text);
    th.appendChild(text);
    row.appendChild(th);
    return text;
}


function tnode(row, text) {
    if (typeof text === 'undefined')
        var text = '';
    var text = document.createTextNode(text);
    var cell = row.insertCell();
    cell.appendChild(text);
    return text;
}


function newTable(caption, cssFloat) {
    var table = document.createElement('table');
    if (typeof caption !== 'undefined') {
        var table_caption = document.createElement('caption');
        table_caption.appendChild(document.createTextNode(caption));
        table.appendChild(table_caption);
    }
    if (typeof caption !== 'undefined') {
        table.style.cssFloat = cssFloat;
    }
    return table;
}


// An actual register.
function Register(name) {
    this.name = name;
    this.value = null;
}


// A list of registers.
function RegisterFile() {
    this.name = 'RegisterFile';
    registerNames = []
    for (var i=0; i < 16; i+=2) {
        registerNames.push('F' + i.toString());
    }
    for (var i=0; i < 8; i++) {
        registerNames.push('R' + i.toString());
    }
    regfile = {}
    for (name of registerNames) {
        regfile[name] = new Register(name);
    }
    regfile['each'] = function*() {
        for (name of registerNames) {
            yield regfile[name]
        }
    }
    return regfile
}

function RegisterStat(registers) {
    var rs = {}
    table = newTable('Register status', 'left');
    var tr = table.insertRow();
    heading(tr, 'Field')
    for (reg of registers.each()) {
        heading(tr, reg.name);
    }
    tr = table.insertRow();
    heading(tr, 'Qi');
    for (reg of registers.each()) {
        rs[reg.name] = {
            'Qi': null,
            '_text': tnode(tr),
            'reg': reg
        };
    }
    document.body.appendChild(table);
    rs['each'] = function*() { for (var r of registers) yield r; }
    return rs;
}


function ExecutionUnit(tr, name) {
    var data = {
        'Name': {
            'value': name,
            '_text': heading(tr, name),
        },
    }
    function makeProperty(name, initialValue) {
        value = typeof initialValue === 'undefined' ? null : initialValue;
        data[name] = {
            'value': value,
            '_text': tnode(tr, value === null ? '' : value.toString()),
        }
        return function(v) {
            if (typeof v === undefined) {
                return data[name]['value']
            }
            data[name]['value'] = v;
            data[name]['_text'].nodeValue = v === null ? '' : v.toString();
        }
    }
    return {
        'Busy': makeProperty('Busy', false),
        'Op': makeProperty('Op', 0),
        'Vj': makeProperty('Vj', 0),
        'Vk': makeProperty('Vk', 0),
        'Qj': makeProperty('Qj', 0),
        'Qk': makeProperty('Qk', 0),
    }
}

var ReservationStation = function(table) {
    var data = {};
    function build(table) {
        tr = table.insertRow();
        for (col of column_names) {
            heading(tr, col);
        }
        return function build_station(name, count) {
            for(var i=0; i < count; i++){
                var tr = table.insertRow();
                var rowName = name + i.toString();
                data[rowName] = new ExecutionUnit(tr, rowName);
            }
        }
    }
    if (typeof table === 'undefined') {
        var table = newTable('Reservation stations', 'right');
        var build_station = build(table);
        build_station('Load', 2);
        build_station('Add', 3);
        build_station('Mult', 2);
        build_station('Store', 2);
        document.body.appendChild(table);
    }
    else {
        data['table'] = table;
    }
    return data;
}

function RINST(op, rd, rs, rt) {
    var data = {
        'op': op,
        'rd': rd,
        'rs': rs,
        'rt': rt,
    }
    return {
        'toString': function() {
            return op + ' ' + rd + ',' + rs + ',' + rt
        },
    }
}

function IINST(op, rd, rs, offset) {
    var data = {
        'op': op,
        'rd': rd,
        'rs': rs,
        'offset': offset,
    }
    return {
        'toString': function() {
            return op + ' ' + rd + ',' + offset.toString() + '(' + rs + ')'
        },
    }
}

function LD(rd, rs, offset) {
    return IINST('L.D', rd, rs, offset);
}

function MULD(rd, rs, rt) {
    return RINST('MUL.D', rd, rs, rt);
}

function DIVD(rd, rs, rt) {
    return RINST('DIV.D', rd, rs, rt);
}

function SD(rd, rs, offset) {
    return IINST('L.D', rd, rs, offset);
}


var newInstruction = function(table, instruction) {
    tr = table.insertRow();
    function box(text) {
        if (typeof text === 'undefined')
            var text = '';
        var text = document.createTextNode(text);
        var cell = tr.insertCell();
        cell.appendChild(text);
        return {'cell': cell, 'text': text};
    }
    var columns = {
        'Instruction': box(instruction.toString()),
        'Issue': box(),
        'Execute': box(),
        'Write result': box(),
    }
}


var InstructionStatus = function(table) {
    if (typeof table === 'undefined') {
        table = newTable('InstructionStatus');
        var tr = table.insertRow();
        for (name of ['Instruction',
                      'From iteration',
                      'Issue',
                      'Execute',
                      'Write result']) {
            heading(tr, name);
        }
        document.body.appendChild(table);
    }
    var instructions = [
        LD('F0', 'R1', 0),
        MULD('F4', 'F0', 'F2'),
        SD('F4', 'R1', 0),
        LD('F0', 'R1', 0),
        MULD('F4', 'F0', 'F2'),
        SD('F4', 'R1', 0),
    ]
    for (var inst of instructions) {
        newInstruction(table, inst);
    }
    return {
        'each': function*() {
            for(var inst of instructions)
                yield inst;
        }
    }
}


var regs = new RegisterFile();
var registerStat = new RegisterStat(regs);
var rs = new ReservationStation();
var is = new InstructionStatus();

for (var i of is.each()) {
    console.log(i);
}

console.debug(rs);
console.debug(registerStat);
