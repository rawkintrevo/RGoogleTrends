# To Do:

## Initial
- [ ] Upload Wiki
- -[ ] Explain how to find topic, date, geo, and category codes
- -[ ] Make better keyword file and explain how to set up keyword file
- -[ ] Requires assumption of fixed search volume. (explain how it works). 
- [ ] Package

## Upgrades
- [ ] Make keywords load from a list (instead of a .csv).  This will probably require doing some guess and check by the script its self (probably an entire sequence)
- [ ] Create function that given average annual search volume (from Adwords) entire matrix of volume can be solved. 
- [ ] Verify volumes against adwords. (Get an idea of margin of error). 
- [ ] Make pivot_word and pivot_name included in scrap gTrends automaitically detect topics and set keyword.
- [ ] Make timeout between queries a settable varaible with warnings/stops for low levels.
- [ ] Do a final random query to check accuracy of results. 
- [ ] Expand state codes to include all 50 states. 
- [ ] Search type support (web/image/etc)
- [ ] Write output to Hbase optionally (this might be a whole other Python project)
