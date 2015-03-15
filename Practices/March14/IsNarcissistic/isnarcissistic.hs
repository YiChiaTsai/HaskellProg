narcissistic = [number |
		x <- [1..9], y <- [0..9], z <- [0..9],
		let number = x*100 + y*10 + z,
		x^3 + y^3 + z^3 == number]
