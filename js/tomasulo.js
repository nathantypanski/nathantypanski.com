var column_names = ['Name', 'Busy', 'Op', 'Vj', 'Vk', 'Qj', 'Qk', 'A'];


function heading(row, text) {
    var th = document.createElement('th');
    var text = document.createTextNode(text);
    th.appendChild(text);
    row.appendChild(th);
    return text;
}


function newTable(caption, cssFloat) {
    var table = document.createElement('table');
    if (typeof caption !== 'undefined') {
        var table_caption = document.createElement('caption');
        table_caption.appendChild(document.createTextNode(caption));
        table.appendChild(table_caption);
    }
    if (typeof cssFloat !== 'undefined') {
        table.style.cssFloat = cssFloat;
    }
    return table;
}


function Address(register, offset) {
    var addr = {
        'register': register,
        'offset': offset,
        'toString': function() {
            if (typeof offset !== 'undefined') {
                var sign = offset < 0 ? ' - ' : ' + ';
                return 'Regs[' + register.toString() + ']'
                    + sign
                    + offset.toString();
            }
            return 'Regs[' + register.toString() + ']'
        }
    }
    return addr;
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
    var table = newTable('Register status');
    $(table).css('position', 'absolute');
    $(table).css('bottom', 0);
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
        var slot = {
            'Qi': function(v) {
                if (typeof v === 'undefined') { return Qi; }
                Qi = v;
                $(cell).text(Qi === null ? '' : Qi.toString());
            },
            // Whether the status slot is full. The opposite of "available".
            'full': function() {
                return Qi !== null;
            },
            // Whether the status slot has no contents.
            'available': function() {
                return Qi === null;
            },
            toString: function() {
                return (Qi === null ? '' : Qi.toString());
            }
        };
        $(cell).mouseenter(function() {
            if (!slot.full()) return;
            $(slot.Qi()._row).find('td').each(function() {
                $(this).css('background', '#f2f2f2');
            });
        });
        $(cell).mouseleave(function() {
            if (!slot.full()) return;
            var $tds = $(slot.Qi()._row).find('td');
            $tds.css('background', '#fff');
        });
        rs[reg.name] = slot;
    }

    for (var reg of registers.each()) {
        makeStatusSlot(reg);
    }
    document.body.appendChild(table);
    rs['each'] = function*() { for (var r of registers) yield r; }
    return rs;
}


function ExecutionUnit(tr, type, name, registerStatus) {
    function makeProperty(name, initialValue, cellCallback) {
        var value = initialValue || null;
        var text = document.createTextNode(value === null ? '' : value.toString());
        var cell = tr.insertCell();
        cell.appendChild(text);
        if (cellCallback)
            cellCallback(cell);
        return function(v) {
            if (typeof v !== 'undefined') {
                value = v;
                $(cell).text(value === null ? '' : v.toString());
            }
            return value;
        }
    }
    var object = {
        type: type,
        _row: tr,
        toString: function() { return name },
        'Name': makeProperty('Name', name),
        'Busy': makeProperty('Busy', false),
        'Op': makeProperty('Op'),
        'Vj': makeProperty('Vj'),
        'Vk': makeProperty('Vk'),
        'A': makeProperty('A'),
    };
    function highlight(selector) {
        return function(cell) {
            $(cell).mouseenter(function() {
                if (!object[selector]()) return;
                console.log(object[selector]());
                $(object[selector]()._row).find('td').each(function() {
                    $(this).css('background', '#f2f2f2');
                });
            });
            $(cell).mouseleave(function() {
                if (!object[selector]()) return;
                $(object[selector]()._row).find('td').each(function() {
                    $(this).css('background', '#fff');
                });
            });
        }
    }
    object.Qj = makeProperty('Qj', null, highlight('Qj'));
    object.Qk = makeProperty('Qk', null, highlight('Qk'));
    object.getSource = function(source) {
        if (registerStatus[source].full() && object.Qj()) {
            object.Qk(registerStatus[source].Qi());
        }
        else if (registerStatus[source].full()) {
            object.Qj(registerStatus[source].Qi());
        }
        else if (object.Vj()) {
            object.Vj(Address(source));
        }
        else {
            object.Vk(Address(source));
        }
    };
    return object;
}

var ReservationStation = function(registerStatus) {
    var data = {};
    var table = newTable('Reservation stations');
    tr = table.insertRow();
    for (var col of column_names) {
        heading(tr, col);
    }
    function build_station(name, count) {
        return _.map(_.range(count), function(i) {
            var tr = table.insertRow();
            var rowName = name + i.toString();
            var unit = new ExecutionUnit(tr, name, rowName, registerStatus);
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
        if (instruction.type !== 'Load') {
            for (var source of instruction.wants()) {
                station.getSource(source);
            }
        }
        else {
            station.A(Address(instruction.rs, instruction.offset));
        }
        return station;
    }
    data.addTable = function() {
        document.body.appendChild(table);
    }
    return data;
}


function Assembler(registerStatus) {
    function RINST(op, rd, rs, rt) {
        return {
            'op': op,
            'rd': rd,
            'rs': rs,
            'rt': rt,
            'toString': function() {
                return op + ' ' + rd + ',' + rs + ',' + rt
            },
            'destination': function() {
                return registerStatus[rd];
            },
            'wants': function() {
                return [rs, rt];
            },
        }
    }
    function IINST(op, rd, rs, offset) {
        return {
            'op': op,
            'rd': rd,
            'rs': rs,
            'offset': offset,
            'toString': function() {
                return op + ' ' + rd + ',' + offset.toString() + '(' + rs + ')'
            },
            'destination': function() {
                return registerStatus[rd];
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
    table = newTable('Instruction Status', 'right');
    var columns = ['Instruction',
                   'Issue',
                   'Execute',
                   'Write result'];

    var newInstruction = function(instruction) {
        tr = table.insertRow();
        var row = {};
        _.each(columns, function(colName) {
            var cell = $(tr.insertCell()).text('');
            row[colName] = cell;
            return cell;
        });
        var issue = function() {
            var reserved = reservationStation.issue(instruction);
            if (reserved) {
                instruction.destination().Qi(reserved);
                row['Issue'].text('true');
            }
        }
        var execute = function () {
            row['Execute'].text('true');
        }
        var writeResult = function () {
            row['Write result'].text('true');
        }
        row['Instruction'].text(instruction.toString()).click(function() {
            if ('true' === row['Write result'].text()) {
                return
            }
            else if ('true' === row['Execute'].text()) {
                writeResult();
            }
            else if ('true' === row['Issue'].text()) {
                execute();
            }
            else {
                issue();
            }
        });
    }

    var tr = table.insertRow();
    for (var name of columns) {
        var th = document.createElement('th');
        var text = document.createTextNode(name);
        th.appendChild(text);
        tr.appendChild(th);
    }
    document.body.appendChild(table);
    console.debug(assembler);
    var instructions = [
        assembler['L.D']('F0', 'R1', 0),
        assembler['MUL.D']('F4', 'F0', 'F2'),
        assembler['S.D']('F4', 'R1', 0),
        assembler['L.D']('F0', 'R1', 8),
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


$(function(){
    var regs = new RegisterFile();
    var registerStat = new RegisterStat(regs);
    var rs = new ReservationStation(registerStat);

    var asm = new Assembler(registerStat);
    var is = new InstructionStatus(asm, rs, registerStat);
    rs.addTable();
});
