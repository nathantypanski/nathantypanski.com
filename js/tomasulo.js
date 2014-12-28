var column_names = ['Name', 'Busy', 'Op', 'Vj', 'Vk', 'Qj', 'Qk', 'A'];


function heading(row, text) {
    var th = document.createElement('th');
    var text = document.createTextNode(text);
    th.appendChild(text);
    row.appendChild(th);
    return text;
}



function tcnode(row, text) {
    if (typeof text === 'undefined')
        var text = '';
    var text = document.createTextNode(text);
    var cell = row.insertCell();
    cell.appendChild(text);
    return {'cell': cell, 'text': text};
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
    _.map(_.range(0, 16, 2), function(i) {
        registerNames.push('F' + i.toString());
    });
    _.map(_.range(0, 8), function(i) {
        registerNames.push('R' + i.toString());
    });

    regfile = {}
    for (var name of registerNames) {
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
    var table = newTable('Register status', 'left');
    var tr = table.insertRow();
    heading(tr, 'Field')
    for (reg of registers.each()) {
        heading(tr, reg.name);
    }
    tr = table.insertRow();
    heading(tr, 'Qi');

    function makeStatusSlot() {
        var Qi = null;
        var text = document.createTextNode('');
        var cell = tr.insertCell();
        cell.appendChild(text);
        rs[reg.name] = {
            'Qi': function(v) {
                if (typeof v === 'undefined') { return value; }
                Qi = v;
                $(cell).text(Qi === null ? '' : Qi.toString());
            },
            'available': function() {
                return Qi === null;
            },
        };
    }

    for (var reg of registers.each()) {
        makeStatusSlot(reg);
    }
    document.body.appendChild(table);
    rs['each'] = function*() { for (var r of registers) yield r; }
    return rs;
}


function ExecutionUnit(tr, type, name) {
    function makeProperty(name, initialValue) {
        var value = typeof initialValue === 'undefined' ? null : initialValue;
        var text = document.createTextNode(value === null ? '' : value.toString());
        var cell = tr.insertCell();
        cell.appendChild(text);
        return function(v) {
            if (typeof v !== 'undefined') {
                value = v;
                $(cell).text(value === null ? '' : v.toString());
            }
            return value;
        }
    }
    var unit =  {
        'type': type,
        'toString': function() { return name },
        'Name': makeProperty('Name', name),
        'Busy': makeProperty('Busy', false),
        'Op': makeProperty('Op'),
        'Vj': makeProperty('Vj'),
        'Vk': makeProperty('Vk'),
        'Qj': makeProperty('Qj'),
        'Qk': makeProperty('Qk'),
        'A': makeProperty('A'),
    }
    return unit;
}

var ReservationStation = function() {
    var data = {};
    var table = newTable('Reservation stations', 'right');
    tr = table.insertRow();
    for (var col of column_names) {
        heading(tr, col);
    }
    function build_station(name, count) {
        return _.map(_.range(count), function(i) {
            var tr = table.insertRow();
            var rowName = name + i.toString();
            var unit = new ExecutionUnit(tr, name, rowName);
            data[rowName] = unit;
            return unit;
        });
    }
    var units = {
        'Load': build_station('Load', 2),
        'Add': build_station('Add', 3),
        'Mult': build_station('Mult', 2),
        'Store': build_station('Store', 2),
    }
    data['issue'] = function(instruction) {
        function busy(executionUnit) {
            return executionUnit.Busy();
        }
        var station = _.first(_.reject(units[instruction.type], busy));
        if (!station) return null;
        station.Busy(true);
        station.Op(instruction.type);
        return station;
    }
    document.body.appendChild(table);
    return data;
}


function Assembler(registerStatus) {
    function RINST(op, rd, rs, rt) {
        var data = {
            'op': op,
            'rd': rd,
            'rs': rs,
            'rt': rt,
        }
        return {
            'op': op,
            'toString': function() {
                return op + ' ' + rd + ',' + rs + ',' + rt
            },
            'destination': function() {
                return registerStat[rd];
            },
            'wants': function() {
                return [rs, rt];
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
            'op': op,
            'toString': function() {
                return op + ' ' + rd + ',' + offset.toString() + '(' + rs + ')'
            },
            'destination': function() {
                return registerStat[rd];
            },
            'wants': function() {
                return [rs];
            },
        }
    }

    return {
        'L.D': function (rd, rs, offset) {
            var i = IINST('L.D', rd, rs, offset);
            i['type'] = 'Load';
            return i;
        },
        'MUL.D': function (rd, rs, rt) {
            var i = RINST('MUL.D', rd, rs, rt);
            i['type'] = 'Mult';
            return i;
        },
        'DIV.D': function (rd, rs, rt) {
            var i = RINST('DIV.D', rd, rs, rt);
            i['type'] = 'Mult';
            return i;
        },
        'S.D': function (rd, rs, offset) {
            var i = IINST('S.D', rd, rs, offset);
            i['type'] = 'Store';
            return i;
        },
    }
}


var InstructionStatus = function(assembler, reservationStation, registerStatus) {
    table = newTable('InstructionStatus');

    var newInstruction = function(instruction) {
        tr = table.insertRow();
        function box(text) {
            if (typeof text === 'undefined')
                var text = '';
            var text = document.createTextNode(text);
            var cell = tr.insertCell();
            cell.appendChild(text);
            $(cell).click(function() {
                if (instruction.destination().available()) {
                    var reserved = reservationStation.issue(instruction);
                    if (reserved) {
                        instruction.destination().Qi(reserved.toString());
                        console.debug(instruction.destination().Qi())
                    }
                }
            });
            return {'cell': cell, 'text': text};
        }
        var columns = {
            'Instruction': box(instruction.toString()),
            'Issue': box(),
            'Execute': box(),
            'Write result': box(),
        }
    }

    var tr = table.insertRow();
    for (name of ['Instruction',
                  'From iteration',
                  'Issue',
                  'Execute',
                  'Write result']) {
        heading(tr, name);
    }
    document.body.appendChild(table);
    console.debug(assembler);
    var instructions = [
        assembler['L.D']('F0', 'R1', 0),
        assembler['MUL.D']('F4', 'F0', 'F2'),
        assembler['S.D']('F4', 'R1', 0),
        assembler['L.D']('F0', 'R1', 0),
        assembler['MUL.D']('F4', 'F0', 'F2'),
        assembler['S.D']('F4', 'R1', 0),
    ]
    for (var inst of instructions) {
        newInstruction(inst);
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

var asm = new Assembler()
var is = new InstructionStatus(asm, rs, registerStat);
