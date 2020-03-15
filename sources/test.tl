
sum(i i32) i32 => {
	if i == 1 {
		ret 1;
	}

	ret i + sum(i - 1);
}

test(i i32) void => {
	printf('%d\n', sum(i));
}

test(100);
