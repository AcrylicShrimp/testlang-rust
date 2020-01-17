
srand(time(0 as u64) as u64);

let a = rand() % 2;

if a == 0 {
	let a = a + 10;

	printf('%d\n', a);
}

printf('%d\n', a);
