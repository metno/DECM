#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
import sys, getopt
from dateutils import relativedelta
from datetime import date

def genDates(firstYear,lastYear):
        time = date(int(firstYear), 1, 1)
        string = time.strftime("%Y%m%d")
        while True:
                time = time + relativedelta(months=1)
                if((time.year > int(lastYear)) or (time > date.today() + relativedelta(months=-2)) ):
                        break
                string = string + "/" + time.strftime("%Y%m%d")
        return string

opts, args = getopt.getopt(sys.argv[1:], "f:l:v:s:t:r:o:")
for opt, param in opts:
        if opt == "-f":
            firstYear = param
        elif opt == "-l":
            lastYear = param
        elif opt == "-v":
            variable = param
        elif opt == "-s":
            steps = param
        elif opt == "-t":
            types = param
        elif opt == "-r":
            stream = param
        elif opt == "-o":
            outfile = param
        else:
            assert False, "unhandled option"

dates = genDates(firstYear,lastYear)    
server = ECMWFDataServer()    
server.retrieve({
    'stream'    : stream,
    "expver"    : "1",
    'param'     : variable,
    'dataset'   : "interim",
    'levtype'   : "sfc",
    'step'      : steps,
    'grid'      : "1.25/1.25",
    'date'      : dates,
#    'time'      : "19790101/19790201/19790301/19790401/19790501/19790601/19790701/19790801/19790901/19791001/19791101/19791201/19800101/19800201/19800301/19800401/19800501/19800601/19800701/19800801/19800901/19801001/19801101/19801201/19810101/19810201/19810301/19810401/19810501/19810601/19810701/19810801/19810901/19811001/19811101/19811201/19820101/19820201/19820301/19820401/19820501/19820601/19820701/19820801/19820901/19821001/19821101/19821201/19830101/19830201/19830301/19830401/19830501/19830601/19830701/19830801/19830901/19831001/19831101/19831201/19840101/19840201/19840301/19840401/19840501/19840601/19840701/19840801/19840901/19841001/19841101/19841201/19850101/19850201/19850301/19850401/19850501/19850601/19850701/19850801/19850901/19851001/19851101/19851201/19860101/19860201/19860301/19860401/19860501/19860601/19860701/19860801/19860901/19861001/19861101/19861201/19870101/19870201/19870301/19870401/19870501/19870601/19870701/19870801/19870901/19871001/19871101/19871201/19880101/19880201/19880301/19880401/19880501/19880601/19880701/19880801/19880901/19881001/19881101/19881201/19890101/19890201/19890301/19890401/19890501/19890601/19890701/19890801/19890901/19891001/19891101/19891201/19900101/19900201/19900301/19900401/19900501/19900601/19900701/19900801/19900901/19901001/19901101/19901201/19910101/19910201/19910301/19910401/19910501/19910601/19910701/19910801/19910901/19911001/19911101/19911201/19920101/19920201/19920301/19920401/19920501/19920601/19920701/19920801/19920901/19921001/19921101/19921201/19930101/19930201/19930301/19930401/19930501/19930601/19930701/19930801/19930901/19931001/19931101/19931201/19940101/19940201/19940301/19940401/19940501/19940601/19940701/19940801/19940901/19941001/19941101/19941201/19950101/19950201/19950301/19950401/19950501/19950601/19950701/19950801/19950901/19951001/19951101/19951201/19960101/19960201/19960301/19960401/19960501/19960601/19960701/19960801/19960901/19961001/19961101/19961201/19970101/19970201/19970301/19970401/19970501/19970601/19970701/19970801/19970901/19971001/19971101/19971201/19980101/19980201/19980301/19980401/19980501/19980601/19980701/19980801/19980901/19981001/19981101/19981201/19990101/19990201/19990301/19990401/19990501/19990601/19990701/19990801/19990901/19991001/19991101/19991201/20000101/20000201/20000301/20000401/20000501/20000601/20000701/20000801/20000901/20001001/20001101/20001201/20010101/20010201/20010301/20010401/20010501/20010601/20010701/20010801/20010901/20011001/20011101/20011201/20020101/20020201/20020301/20020401/20020501/20020601/20020701/20020801/20020901/20021001/20021101/20021201/20030101/20030201/20030301/20030401/20030501/20030601/20030701/20030801/20030901/20031001/20031101/20031201/20040101/20040201/20040301/20040401/20040501/20040601/20040701/20040801/20040901/20041001/20041101/20041201/20050101/20050201/20050301/20050401/20050501/20050601/20050701/20050801/20050901/20051001/20051101/20051201/20060101/20060201/20060301/20060401/20060501/20060601/20060701/20060801/20060901/20061001/20061101/20061201/20070101/20070201/20070301/20070401/20070501/20070601/20070701/20070801/20070901/20071001/20071101/20071201/20080101/20080201/20080301/20080401/20080501/20080601/20080701/20080801/20080901/20081001/20081101/20081201/20090101/20090201/20090301/20090401/20090501/20090601/20090701/20090801/20090901/20091001/20091101/20091201/20100101/20100201/20100301/20100401/20100501/20100601/20100701/20100801/20100901/20101001/20101101/20101201/20110101/20110201/20110301/20110401/20110501/20110601/20110701/20110801/20110901/20111001/20111101/20111201/20120101/20120201/20120301/20120401/20120501/20120601/20120701/20120801/20120901/20121001/20121101/20121201/20130101/20130201/20130301/20130401/20130501/20130601/20130701/20130801/20130901/20131001/20131101/20131201/20140101/20140201/20140301/20140401/20140501/20140601/20140701/20140801/20140901/20141001/20141101/20141201/20150101/20150201/20150301/20150401/20150501/20150601/20150701/20150801/20150901/20151001/20151101/20151201/20160101/20160201/20160301/20160401/20160501/20160601/20160701/20160801/20160901/20161001/20161101/20161201",
    'type'      : types,
    'class'     : "ei",
    'target'    : outfile
})
