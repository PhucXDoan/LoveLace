const cssTitle   = "font-size : 30px; font-weight : bold; text-decoration: underline"
const cssSection = "font-size : 25px; font-weight : bold; text-decoration: none; font-style : italic"
const cssItems   = "font-size : 15px; font-weight : normal"
const menus      =
	List (
`%cAngles%c
	A+1. Angle → ( 90°) Supplementary of Angle
	A+2. Angle → (180°) Complementary of Angle
	A+3. Radians → Degrees
	A-3. Degrees → Radians
`,

`%cCircles%c
	C+1. Radius        → Area
	C+2. Radius        → Circumference
	C+3. Diameter      → Area
	C+4. Diameter      → Circumference
	C-1. Area          → Radius
	C-2. Circumference → Radius
	C-3. Area          → Diameter
	C-4. Circumference → Diameter
`,

`%cPolygons%c
	P+1. N                           → Int. Angle of Reg. N-gon?
	P+2. N                           → Ext. Angle of Reg. N-gon?
	P+3. N                           → Sum of Int. Angles of N-gon?
	P-1. Int. Angle of Reg. N-gon    → N?
	P-2. Ext. Angle of Reg. N-gon    → N?
	P-3. Sum of Int. Angles of N-gon → N?
`
	)

const audios =
	List (
		'./audio/click_mechanical.mp3',
		'./audio/coin_insert.mp3',
		'./audio/beep_harsh.mp3',
		'./audio/vendingmachine_drop.mp3',
		'./audio/windowsxp_shutdown.mp3'
	)

const fix = formatter (roundToString (3))

const requestNumber = (message : string) : IOMaybe <number> =>
	prompting (message)
		.pipe (IOM.merge)
		.bind (input =>
			readFloat (input)
				.fmap (sendJust)
				.pipe (fromMaybe (
					playAudio ('./audio/beep_harsh.mp3')
						.then (alerting (
							trim (input) === ""
								? `The Vending Ma(th)chine didn't receive any input. It is not telepathic.`
								: `The Vending Ma(th)chine didn't understand the input "${input}".`
						))
						.pipe (IOM.lift)
						.then (requestNumber (message))
				))
		)

const shortcircuit = (message : string) => (computeWork : (num : number) => string) : IO <null> =>
	IOM.lift (playAudio ('./audio/coin_insert.mp3'))
		.then (requestNumber (message))
			.fmap (computeWork)
			.side (IOM.lift (playAudio ('./audio/vendingmachine_drop.mp3')))
			.bind (IOM.liftf (alerting))
		.pipe (IOM.resolve)

const circuit = (code : string) : IO <null> =>
	code === 'A+1' ?
		shortcircuit
			("What's the angle (degrees)?")
			(a => fix `${a}\n90-${a}\n${90 - a}`)
	:
	code === 'A+2' ?
		shortcircuit
			("What's the angle (degrees)?")
			(a => fix `${a}\n180-${a}\n${180 - a}`)
	:
	code === 'A+3' ?
		shortcircuit
			("What's the angle in Radians?")
			(a => fix `${a}*180/pi = ${a * 180}/pi = ${a * 180 / pi}`)
	:
	code === 'A-3' ?
		shortcircuit
			("What's the angle in Degrees?")
			(a => fix `${a}/180*pi = ${a / 180}*pi = ${a / 180 * pi}`)
	:
	code === 'C+1' ?
		shortcircuit
			("What's the radius?")
			(r => fix `pi*r^2 = pi*${r}^2 = pi*${r ** 2} = ${pi * r ** 2}`)
	:
	code === 'C+2' ?
		shortcircuit
			("What's the radius?")
			(r => fix `2*pi*r = 2*pi*${r} = 2*${pi * r} = ${2 * pi * r}`)
	:
	code === 'C+3' ?
		shortcircuit
			("What's the diameter?")
			(d => fix `pi*(d/2)^2 = pi*(${d}/2)^2 = pi*(${d / 2})^2 = pi*${d / 2 ** 2} = ${pi * (d / 2) ** 2}`)
	:
	code === 'C+4' ?
		shortcircuit
			("What's the diameter?")
			(d => fix `pi*d = pi*${d} = ${pi*d}`)
	:
	code === 'C-1' ?
		shortcircuit
			("What's the area?")
			(a => fix `pi*r^2 = ${a}\nr^2 = ${a}/pi\nr^2 = ${a / pi}\nr = sqrt(${a / pi})\nr = ${sqrt (a / pi)}`)
	:
	code === 'C-2' ?
		shortcircuit
			("What's the circumference?")
			(c => fix `2*pi*r = ${c}\npi*r = ${c}/2\npi*r = ${c / 2}\nr = ${c / 2 / pi}`)
	:
	code === 'C-3' ?
		shortcircuit
			("What's the area?")
			(a => fix `pi*(d/2)^2 = ${a}\n(d/2)^2 = ${a}/pi\n(d/2)^2 = ${a / pi}\nd/2 = sqrt(${a / pi})\nd/2 = ${sqrt(a / pi)}\nd = ${sqrt(a / pi) * 2}`)
	:
	code === 'C-4' ?
		shortcircuit
			("What's the circumference")
			(c => fix `pi*d = ${c}\nd = ${c} / pi\nd = ${c / pi}\n`)
	:
	code === 'C-4' ?
		shortcircuit
			("What's the circumference")
			(c => fix `pi*d = ${c}\nd = ${c} / pi\nd = ${c / pi}\n`)
	:
	code === 'P+1' ?
		shortcircuit
			("How many sides of the polygon are there?")
			(n => fix `180*(n-2)/n = 180*(${n}-2)/n = 180*(${n - 2})/n = ${180 * (n - 2)}/n = ${180 * (n - 2) / n}`)
	:
	code === 'P+2' ?
		shortcircuit
			("How many sides of the polygon are there?")
			(n => fix `360/n = 360/${n} = ${360 / n}`)
	:
	code === 'P+3' ?
		shortcircuit
			("How many sides of the polygon are there?")
			(n => fix `180*(n-2) = 180*(${n}-2) = 180*(${n - 2}) = ${180 * (n - 2)}`)
	:
	code === 'P-1' ?
		shortcircuit
			("What is the interior angle?")
			(a => fix `180*(n-2)/n = ${a}\n180*(1-2/n) = ${a}\n(1-2/n) = ${a}/180\n1-2/n = ${a / 180}\n-2/n = ${a / 180}-1\n-2/n = ${a / 180 - 1}\n-2 = n*(${a / 180 - 1})\n-2/(${a / 180 - 1}) = n\n${-2 / (a / 180 - 1)} = n`)
	:
	code === 'P-2' ?
		shortcircuit
			("What is the exterior angle?")
			(a => fix `360/n = ${a}\n360 = n*${a}\n360/${a} = n\n${360 / a} = n\n`)
	:
	code === 'P-3' ?
		shortcircuit
			("What is the sum of the interior angles?")
			(s => fix `180*(n-2) = ${s}\nn-2 = ${s}/180\nn-2 = ${s / 180}\nn = ${s / 180}+2\nn = ${s / 180 + 2}`)
	:
	trim (code) === "" ?
		idle
	:
		playAudio ('./audio/beep_harsh.mp3')
			.then (alerting (`The Vending Ma(th)chine didn't understand the code "${code}".`))

const machine : IO <null> =
	playSFX ('./audio/click_mechanical.mp3')
		.then (prompting ("The Vending Ma(th)chine™ asks for a code: "))
		.pipe (IOM.merge)
			.fmap (removeRegex (/\s/g))
			.fmap (uppercase)
			.bind (IOM.liftf (circuit))
			.bind (_ => IOM.lift (machine))
		.pipe (IOM.fromIOMaybe (playAudio ('./audio/windowsxp_shutdown.mp3')))

const main =
	audios
		.fmap (loadAudio)
		.pipe (execute_IO)
		.then (printf ("%cMath Vending Ma(th)chine", cssTitle))
		.then (
			menus
				.fmap (menu => printf (menu, cssSection, cssItems))
				.pipe (execute_IO)
		)
		.then (machine)
