
let foo = rand();

printf('foo = %d\n', foo);

if foo < 10 {
	printf('foo < 10\n');
} else if foo < 100 {
	printf('foo < 100\n');
} else if foo < 1000 {
	printf('foo < 1000\n');
} else if foo < 10000 {
	printf('foo < 10000\n');
} else if foo < 100000 {
	printf('foo < 100000\n');
} else if foo < 1000000 {
	printf('foo < 1000000\n');
} else {
	printf('foo >= 1000000\n');
}
