var column_names = [
    'Name',
    'Busy',
    'Op',
    'Vj',
    'Vk',
    'Qj',
    'Qk',
    'A',
    'Result'
];


// Make all functions automatically curryable..
Function.prototype.curry = function () {
    var slice = Array.prototype.slice,
        args = slice.apply(arguments),
        that = this;
    return function ( ) {
        return that.apply(null, args.concat(slice.apply(arguments)));
    };
};


function defined(v) {
    return typeof v !== 'undefined';
}


function heading(row, words, tooltip) {
    var heading = {'text': words};

    if (defined(tooltip)) {
        heading['title'] = tooltip;
        heading['class'] = 'mastertooltip';
    }
    $(row).append($('<th>', heading));
}


function newTable(kwargs) {
    var table = document.createElement('table');
    if (defined(kwargs['caption'])) {
        var table_caption = document.createElement('caption');
        table_caption.appendChild(document.createTextNode(kwargs['caption']));
        table.appendChild(table_caption);
    }
    if (defined(kwargs['id'])) {
        $(table).attr('id', kwargs['id']);
    }
    return table;
}


function Address(register, offset) {
    var addr = {
        'register': register,
        'offset': offset,
        'toString': function() {
            if (defined(offset)) {
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
        for (var name of registerNames) {
            yield regfile[name]
        }
    }
    return regfile
}


function RegisterStat(registers) {
    var rs = {}
    var table = newTable({
        'caption': 'Register status',
        'id': 'RegisterStatus',
    });
    var tr = table.insertRow();

    heading(tr, 'Field')
    for (var reg of registers.each()) {
        heading(tr, reg.name);
    }
    tr = table.insertRow();
    heading(tr, 'Qi', 'Which reservation station will produce register contents');

    function makeStatusSlot(reg) {
        var Qi = null;
        var text = document.createTextNode('');
        var cell = tr.insertCell();
        cell.appendChild(text);
        var slot = {
            'name': reg.name,
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
            },
            'Qi': function(v) {
                if (typeof v === 'undefined') { return Qi; }
                Qi = v;
                $(cell).text(Qi === null ? '' : Qi.toString());
            },
        };
        slot.highlightSource = function() {
            if (slot.full())
                $(cell).addClass('sourceOccupied');
            else
                $(cell).addClass('source');
        };
        slot.highlightDestination = function() {
            if (slot.full())
                $(cell).addClass('destinationOccupied');
            else
                $(cell).addClass('destination');
        };
        slot.clearHighlights = function() {
            $(cell).attr('class', '');
        };
        $(cell).hover(function() {
            if (!slot.full()) return;
            $(slot.Qi()._row).find('td').each(function() {
                $(this).toggleClass('source');
            });
        });
        rs[reg.name] = slot;
        return slot;
    }

    registerSlots = [];
    for (var reg of registers.each()) {
        registerSlots.push(makeStatusSlot(reg));
    }
    document.body.appendChild(table);
    rs.each = function*() { for (var r of registerSlots) yield r; }
    return rs;
}


function ExecutionUnit(tr, type, name, registerStatus) {
    function makeProperty(name, initialValue, cellCallback) {
        var value = defined(initialValue) ? initialValue : null;
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
        // Type of this execution unit.
        //
        // 'Load'  -> memory load
        // 'Add'   -> add/subtract
        // 'Mult'  -> mult/div
        // 'Store' -> memory store
        type: type,
        _row: tr,
        toString: function() { return name },
        'Name': makeProperty('Name', name),
        'Busy': makeProperty('Busy', false),
        'Op': makeProperty('Op'),
        'Vj': makeProperty('Vj'),
        'Vk': makeProperty('Vk'),
    };

    function highlight(selector) {
        return function(cell) {
            $(cell).hover(function() {
                if (!object[selector]()) return;
                $(object[selector]()._row).find('td').each(function() {
                    $(this).toggleClass('highlight');
                });
            });
        }
    }

    object.instruction = null;
    object.Qj = makeProperty('Qj', null, highlight('Qj'));
    object.Qk = makeProperty('Qk', null, highlight('Qk'));
    object.A = makeProperty('A', '');
    object.Result = makeProperty('Result');
    object.getSource = function(source) {
        if (source.full() && object.Qj()) {
            object.Qk(source.Qi());
        }
        else if (source.full()) {
            object.Qj(source.Qi());
        }
        else if (object.Vj()) {
            object.Vj(Address(source.name));
        }
        else {
            object.Vk(Address(source.name));
        }
    };
    object.issue = function(instruction) {
        if (object.Busy()) {
            return null;
        }
        object.instruction = instruction;
        object.Busy(true);
        object.Op(instruction.type);
        if (instruction.type === 'Store') {
            object.A(Address(instruction.rd, instruction.offset));
            for (var source of instruction.wants()) {
                object.getSource(source);
            }
        }
        else {
            for (var source of instruction.wants()) {
                object.getSource(source);
            }
        }
        return object;
    }
    object.execute = function() {
        if (object.instruction.type === 'Load' && object.A() === '') {
            // Load step 1.
            //
            // Wait until RS[r].Qj = 0 & r is head of load-store queue:
            //
            //    RS[r].A <- RS[r].Vj + RS[r].A;
            //
            object.A(Address(object.instruction.rs, object.instruction.offset));
            return;
        } else if (!object.instruction || object.Qj() || object.Qk() || !object.Busy()) {
            return null;
        }
        object.Vj(null)
        object.Vk(null)
        object.Result(true)
        return object;
    }

    // We are waiting on the execution unit if it is our Qi or Qj source.
    object.waitingOn = function(executionUnit) {
        // Shortcut the operation if Qi() or Qj() are null;
        if (null === object.Qj() || null === object.Qk())
            return false;
        return executionUnit === object.Qj() || executionUnit === object.Qk();
    };
    object.resolve = function(executionUnit) {
        if (executionUnit === object.Qj()) {
            object.Qj(null);
            object.Vj(true);
        }
        if (executionUnit === object.Qk()) {
            object.Qk(null);
            object.Vk(true);
        }
        return object;
    };
    object.clear = function() {
        object.instruction = null;
        object.A(null)
        object.Result(null)
        object.Op(null)
        object.Qj(null)
        object.Qk(null)
        object.Vj(null)
        object.Vk(null)
        object.Busy(false);
        _.each(registerStatus.each(), function(rs) {
            if (rs.Qi() === object)
                rs.Qi(null);
        });
        return object;
    };
    return object;
}


// Construct a reservation station.
//
// Args:
//   registerStatus: the register status object.
var ReservationStation = function(registerStatus) {
    var object = {};

    var table = newTable({
        'caption': 'Reservation stations',
        'id': 'ReservationStations',
    });

    tr = table.insertRow();
    heading(tr, 'Name', 'Name of the execution unit');
    heading(tr, 'Busy', 'Whether the unit is in use');
    heading(tr, 'Op', 'Operation type');
    heading(tr, 'Vj', 'j value');
    heading(tr, 'Vk', 'k value');
    heading(tr, 'Qj', 'j data source');
    heading(tr, 'Qk', 'k data source');
    heading(tr, 'A', 'Calculated address');
    heading(tr, 'Result', 'Whether a final result has been produced');

    var executionUnits = [];

    function build_station(name, quantity) {
        return _.map(_.range(quantity), function(i) {
            var tr = table.insertRow();
            // i is the number of execution units already created of this
            // type. So the rowName is the execution unit type name along
            // with its number.
            var rowName = name + i.toString();
            var unit = new ExecutionUnit(tr, name, rowName, registerStatus);
            object[rowName] = unit;
            executionUnits.push(unit);
            return unit;
        });
    }

    var units = {
        'Load': build_station('Load', 2),
        'Add': build_station('Add', 3),
        'Mult': build_station('Mult', 2),
        'Store': build_station('Store', 2),
    }

    object.issue = function(instruction) {
        currentInstruction = instruction;
        function busy(executionUnit) {
            return executionUnit.Busy();
        }
        var station = _.first(_.reject(units[instruction.type], busy));
        if (!station) return null;
        else station.issue(instruction);
        return station;
    };

    object.addTable = function() {
        document.body.appendChild(table);
    };

    object.each = function*() {
        for(var unit of executionUnits)
            yield unt;
    }

    object.writeBack = function(finishedUnit) {
        if (!finishedUnit.Busy()) {
            throw 'Execution Unit attempted writeback without instruction';
        }
        for(var unit of executionUnits) {
            unit.resolve(finishedUnit);
        }
        finishedUnit.clear();
        return finishedUnit;
    }

    return object;
}


function Assembler(registerStatus) {
    // R-type (register) instruction
    function RINST(op, type, rd, rs, rt) {
        return {
            'op': op,
            'type': type,
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
                return [registerStatus[rs], registerStatus[rt]];
            },
        }
    }

    // I-type (immediate) instruction
    function IINST(op, type, rd, rs, offset) {
        if (type === 'Store') {
            var temp = rd;
            rd = rs;
            rs = temp;
        }
        return {
            'op': op,
            'type': type,
            'rd': rd,
            'rs': rs,
            'offset': offset,
            'toString': function() {
                if (type !== 'Store')
                    return op + ' ' + rd + ',' + offset.toString() + '(' + rs + ')'
                else
                    return op + ' ' + rs + ',' + offset.toString() + '(' + rd + ')'
            },
            'destination': function() {
                return registerStatus[rd];
            },
            'wants': function() {
                return [registerStatus[rs]];
            },
        }
    }

    return {
        'LD': IINST.curry('L.D').curry('Load'),
        'SUBD': RINST.curry('SUB.D').curry('Add'),
        'ADDD': RINST.curry('ADD.D').curry('Add'),
        'MULD': RINST.curry('MUL.D').curry('Mult'),
        'DIVD': RINST.curry('DIV.D').curry('Mult'),
        'SD': IINST.curry('S.D').curry('Store'),
    }
}


var InstructionStatus = function(reservationStation, registerStatus) {
    table = newTable({'caption': 'Instruction Status'});
    $(table).attr('id', 'InstructionStatus');
    var columns = ['Instruction',
                   'Issue',
                   'Execute',
                   'Write result'];


    var tr = table.insertRow();
    for (var name of columns) {
        var th = document.createElement('th');
        var text = document.createTextNode(name);
        th.appendChild(text);
        tr.appendChild(th);
    }
    document.body.appendChild(table);
    return {
        'newInstruction': function(instruction) {
            tr = table.insertRow();
            var row = {};
            var executionUnit = null;
            var cells = _.each(columns, function(colName) {
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
                return reserved;
            }
            var execute = function () {
                var executed = executionUnit.execute();
                if (executed) {
                    row['Execute'].text('true');
                }
                return executed;
            }
            var writeResult = function () {
                var wrote = reservationStation.writeBack(executionUnit);
                if (wrote) {
                    if (instruction.destination().Qi() === executionUnit) {
                        instruction.destination().Qi(null);
                    }
                    row['Write result'].text('true');
                    executionUnit = null;
                }
                return wrote;
            }

            function addHighlights() {
                $(this).addClass('highlight');
                if (!row['Issue'].text()) {
                    instruction.destination().highlightDestination();
                    for (var source of instruction.wants()) {
                        source.highlightSource();
                    }
                }
            }

            function clearHighlights() {
                $(this).removeClass('highlight');
                instruction.destination().clearHighlights();
                for (var source of instruction.wants()) {
                    source.clearHighlights();
                }
            }

            row['Instruction'].hover(addHighlights, clearHighlights);

            row['Instruction'].text(instruction.toString()).click(function() {
                if ('true' === row['Write result'].text()) {
                    return
                }
                else if ('true' === row['Execute'].text()) {
                    if(writeResult(executionUnit))
                        executionUnit = null;
                }
                else if ('true' === row['Issue'].text()) {
                    execute(executionUnit);
                }
                else {
                    executionUnit = issue();
                }
                clearHighlights();
            });
        },
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

    $('.mastertooltip').hover(function(){
            // Hover over code
            var title = $(this).attr('title');
            $(this).data('tipText', title).removeAttr('title');
            $('<p>', {
                'text': title,
                'class': 'tooltip',
            }).appendTo('body').fadeIn('slow');
    }, function() {
            // Hover out code
            $(this).attr('title', $(this).data('tipText'));
            $('.tooltip').remove().fadeOut('fast');
    }).mousemove(function(e) {
            var mousex = e.pageX + 16; //Get X coordinates
            var mousey = e.pageY - 32; //Get Y coordinates
            $('.tooltip')
            .css({ top: mousey , left: mousex });
    });

    var instructions = [
        // asm.LD('F0', 'R1', 0),
        // asm.MULD('F4', 'F0', 'F2'),
        // asm.SD('F4', 'R1', 0),
        // asm.LD('F0', 'R1', 8),
        // asm.MULD('F4', 'F0', 'F2'),
        // asm.SD('F4', 'R1', 0),
        asm.LD('F6', 'R2', 32),
        asm.LD('F2', 'R3', 44),
        asm.MULD('F0', 'F2', 'F4'),
        asm.SUBD('F8', 'F2', 'F6'),
        asm.DIVD('F10', 'F0', 'F6'),
        asm.ADDD('F6', 'F8', 'F2'),
    ]
    for (var inst of instructions) {
        is.newInstruction(inst);
    }
});
