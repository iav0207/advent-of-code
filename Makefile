
toc:
	./toc.sh

lang=kotlin

day:
	./init_day.sh $(year) $(day) $(lang)

day_kt: lang=kotlin
day_kt: day $(year) $(day)

