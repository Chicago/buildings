import csv
import usaddress
import sys

addr = str(sys.argv[1:])
parsed = usaddress.parse(addr)
with open("parsed.csv",'w') as resultFile:
  wr = csv.writer(resultFile)
  wr.writerows(parsed)
