
srand(time(0));

let foo = rand() % 2;
let bar = rand() % 2;
let baz = rand() % 2;

if foo == 0 and bar == 0 and baz == 0 {
	printf('000\n');
} else if foo == 0 and bar == 0 and baz == 1 {
	printf('001\n');
} else if foo == 0 and bar == 1 and baz == 0 {
	printf('010\n');
} else if foo == 0 and bar == 1 and baz == 1 {
	printf('011\n');
} else if foo == 1 and bar == 0 and baz == 0 {
	printf('100\n');
} else if foo == 1 and bar == 0 and baz == 1 {
	printf('101\n');
} else if foo == 1 and bar == 1 and baz == 0 {
	printf('110\n');
} else if foo == 1 and bar == 1 and baz == 1 {
	printf('111\n');
}
