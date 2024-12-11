# -*- coding: utf-8 -*-
# Problem Set 5: Experimental Analysis
# Name: 
# Collaborators (discussion):
# Time:
import numpy as np
import pylab
import matplotlib.pyplot as plt
import math
import re

# cities in our weather data
CITIES = [
    'BOSTON',
    'SEATTLE',
    'SAN DIEGO',
    'PHILADELPHIA',
    'PHOENIX',
    'LAS VEGAS',
    'CHARLOTTE',
    'DALLAS',
    'BALTIMORE',
    'SAN JUAN',
    'LOS ANGELES',
    'MIAMI',
    'NEW ORLEANS',
    'ALBUQUERQUE',
    'PORTLAND',
    'SAN FRANCISCO',
    'TAMPA',
    'NEW YORK',
    'DETROIT',
    'ST LOUIS',
    'CHICAGO'
]

TRAINING_INTERVAL = range(1961, 2010)
TESTING_INTERVAL = range(2010, 2016)

"""
Begin helper code
"""
class Climate(object):
    """
    The collection of temperature records loaded from given csv file
    """
    def __init__(self, filename):
        """
        Initialize a Climate instance, which stores the temperature records
        loaded from a given csv file specified by filename.

        Args:
            filename: name of the csv file (str)
        """
        self.rawdata = {}

        f = open(filename, 'r')
        header = f.readline().strip().split(',')
        for line in f:
            items = line.strip().split(',')

            date = re.match('(\d\d\d\d)(\d\d)(\d\d)', items[header.index('DATE')])
            year = int(date.group(1))
            month = int(date.group(2))
            day = int(date.group(3))

            city = items[header.index('CITY')]
            temperature = float(items[header.index('TEMP')])
            if city not in self.rawdata:
                self.rawdata[city] = {}
            if year not in self.rawdata[city]:
                self.rawdata[city][year] = {}
            if month not in self.rawdata[city][year]:
                self.rawdata[city][year][month] = {}
            self.rawdata[city][year][month][day] = temperature
            
        f.close()

    def get_yearly_temp(self, city, year):
        """
        Get the daily temperatures for the given year and city.

        Args:
            city: city name (str)
            year: the year to get the data for (int)

        Returns:
            a 1-d pylab array of daily temperatures for the specified year and
            city
        """
        temperatures = []
        assert city in self.rawdata, "provided city is not available"
        assert year in self.rawdata[city], "provided year is not available"
        for month in range(1, 13):
            for day in range(1, 32):
                if day in self.rawdata[city][year][month]:
                    temperatures.append(self.rawdata[city][year][month][day])
        return pylab.array(temperatures)

    def get_daily_temp(self, city, month, day, year):
        """
        Get the daily temperature for the given city and time (year + date).

        Args:
            city: city name (str)
            month: the month to get the data for (int, where January = 1,
                December = 12)
            day: the day to get the data for (int, where 1st day of month = 1)
            year: the year to get the data for (int)

        Returns:
            a float of the daily temperature for the specified time (year +
            date) and city
        """
        assert city in self.rawdata, "provided city is not available"
        assert year in self.rawdata[city], "provided year is not available"
        assert month in self.rawdata[city][year], "provided month is not available"
        assert day in self.rawdata[city][year][month], "provided day is not available"
        return self.rawdata[city][year][month][day]

def se_over_slope(x, y, estimated, model):
    """
    For a linear regression model, calculate the ratio of the standard error of
    this fitted curve's slope to the slope. The larger the absolute value of
    this ratio is, the more likely we have the upward/downward trend in this
    fitted curve by chance.
    
    Args:
        x: an 1-d pylab array with length N, representing the x-coordinates of
            the N sample points
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        estimated: an 1-d pylab array of values estimated by a linear
            regression model
        model: a pylab array storing the coefficients of a linear regression
            model

    Returns:
        a float for the ratio of standard error of slope to slope
    """
    assert len(y) == len(estimated)
    assert len(x) == len(estimated)
    EE = ((estimated - y)**2).sum()
    var_x = ((x - x.mean())**2).sum()
    SE = pylab.sqrt(EE/(len(x)-2)/var_x)
    return SE/model[0]

"""
End helper code
"""

def generate_models(x, y, degs):
    """
    Generate regression models by fitting a polynomial for each degree in degs
    to points (x, y).

    Args:
        x: an 1-d pylab array with length N, representing the x-coordinates of
            the N sample points
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        degs: a list of degrees of the fitting polynomial

    Returns:
        a list of pylab arrays, where each array is a 1-d array of coefficients
        that minimizes the squared error of the fitting polynomial
    """
    # TODO
    #y_float=y.astype(np.float64)  # convert all to float data type
    #x_float=x.astype(np.float64)
    L_out=[]
    count=1
    while count <= len(degs):
        L_out.append(np.array(np.polyfit(x,y,degs[count-1])))
        count +=1
    print(L_out)    
    return L_out

def r_squared(y, estimated):
    """
    Calculate the R-squared error term.
    
    Args:
        y: 1-d pylab array with length N, representing the y-coordinates of the
            N sample points
        estimated: an 1-d pylab array of values estimated by the regression
            model

    Returns:
        a float for the R-squared error term
    """
    # TODO
    # calculate the numerator sum of square
    # calculate the numerator sum of square
    #estimated = float(estimated)
    #y = float (y)
    estimated_float = estimated.astype(np.float64)
    y_float=y.astype(np.float64)
#print("  y float",y_float.dtype)
    
    # calculate the numerator sum of square
    # calculate the numerator sum of square
    arr_num = y_float - estimated_float
    arr_num = pylab.square(arr_num)
    sum_num = pylab.sum(arr_num)
    #print(y_float)
    
    # calculate average of y
    y_avg = float(pylab.average(y))
    print(y_avg)
    arr_avg = estimated_float #use estimated because fill will alter the numbers
    arr_avg.fill(y_avg)
    #print(arr_avg)
    arr_denom = y_float - arr_avg
    arr_denom = pylab.square(arr_denom)
    sum_denom = pylab.sum(arr_denom)
    #return R square
    r_sq = 1 - (sum_num/sum_denom)
    return r_sq

def evaluate_models_on_training(x, y, models):
    """
    For each regression model, compute the R-squared value for this model with the
    standard error over slope of a linear regression line (only if the model is
    linear), and plot the data along with the best fit curve.

    For the plots, you should plot data points (x,y) as blue dots and your best
    fit curve (aka model) as a red solid line. You should also label the axes
    of this figure appropriately and have a title reporting the following
    information:
        degree of your regression model,
        R-square of your model evaluated on the given data points,
        and SE/slope (if degree of this model is 1 -- see se_over_slope). 

    Args:
        x: an 1-d pylab array with length N, representing the x-coordinates of
            the N sample points
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        models: a list containing the regression models you want to apply to
            your data. Each model is a pylab array storing the coefficients of
            a polynomial.

    Returns:
        None
    """
    # TODO
    i=0
    modNumber = len(models)
    print('Print models',modNumber,'on Training data')
    while i < (modNumber):
        #string_rsquare ='rsquare'
        plt.xlabel(' year ')
        plt.ylabel('degrees Celsius')
        plt.plot(x,y, 'bo')
        estimated = np.array(pylab.polyval(models[i],x))
        plt.plot(x,estimated, 'r--', label =' model')
        string_degree =str(len(models[i])-1)
        string_rsquare = str(r_squared(y, estimated))
        plt.title('Degree:' + string_degree+ '\n  Rsquare:' + string_rsquare)
        plt.show()
        i += 1

def gen_cities_avg(climate, multi_cities, years):
    """
    Compute the average annual temperature over multiple cities.

    Args:
        climate: instance of Climate
        multi_cities: the names of cities we want to average over (list of str)
        years: the range of years of the yearly averaged temperature (list of
            int)

    Returns:
        a pylab 1-d array of floats with length = len(years). Each element in
        this array corresponds to the average annual temperature over the given
        cities for a given year.
    """
    # TODO
    tempCitiesArr=[]
    count=0
    while count < len(years):
        i=0
        cityTmpArr=[]
        while i < len(multi_cities):
                yr_int=int(years[count])
                yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
                yrAvg = pylab.mean(yrTempArr)
                cityTmpArr.append(yrAvg)
                i +=1
        #print('cityTmpArr =',cityTmpArr, '\n')
        citiesAvg = pylab.mean(cityTmpArr)
        #print('Cities Avg =',citiesAvg,'\n')
        tempCitiesArr.append(citiesAvg)
        count +=1
    return tempCitiesArr

def moving_average(y, window_length):
    """
    Compute the moving average of y with specified window length.

    Args:
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        window_length: an integer indicating the window length for computing
            moving average

    Returns:
        an 1-d pylab array with the same length as y storing moving average of
        y-coordinates of the N sample points
    """
    # TODO
    #window_length = window_length -1
    movingAvgArr=[]
    count=0
    win_int=int(window_length)
    while count < len(y):
        if count < win_int:
            first = 0
        else:
            first = count - win_int + 1
            
        if (count ==0) and (first ==0):
            movingAvgArr.append(y[0])
        else:
            movingAvgArr.append(pylab.mean(y[first:(count+1)]))
    #print(first,'count',count, ' = ',y[first],' - ',y[count])
        count +=1
    #movingAvgArr=movingAvgArr[1:]     #cut out the first nan element
    return movingAvgArr

def rmse(y, estimated):
    """
    Calculate the root mean square error term.

    Args:
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        estimated: an 1-d pylab array of values estimated by the regression
            model

    Returns:
        a float for the root mean square error term
    """
    # TODO
    estimated_float = estimated.astype(np.float64)
    y_float=y.astype(np.float64)
    arr_num = y_float - estimated_float
    arr_num = pylab.square(arr_num)
    sum_num = pylab.sum(arr_num)
    rmse = math.sqrt(sum_num / (len(estimated)))
    return rmse
def gen_std_devs(climate, multi_cities, years):
    """
    For each year in years, compute the standard deviation over the averaged yearly
    temperatures for each city in multi_cities. 

    Args:
        climate: instance of Climate
        multi_cities: the names of cities we want to use in our std dev calculation (list of str)
        years: the range of years to calculate standard deviation for (list of int)

    Returns:
        a pylab 1-d array of floats with length = len(years). Each element in
        this array corresponds to the standard deviation of the average annual 
        city temperatures for the given cities in a given year.
    """
 # TODO
    tempCitiesArr=[]
    count=0
    while count < len(years):
        i=0
        yr_int=int(years[count])
        yrTotalArr=climate.get_yearly_temp(multi_cities[i],yr_int)  #get the array for the first yr
        i=1
        while i < len(multi_cities):
                #yr_int=int(years[count])
                yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
                yrTotalArr = yrTotalArr + yrTempArr
                i +=1
        yrAvgArr = yrTotalArr / int(len(multi_cities))
        #print('cityTmpArr =',yrAvgArr, '\n')
        citiesStd = pylab.std(yrAvgArr)
        #print('Cities Avg =',citiesAvg,'\n')
        tempCitiesArr.append(citiesStd)
        count +=1
    return tempCitiesArr
 
"""
    stdCitiesArr=[]
    #tempCitiesArr=[]
    count=0
    while count < len(years):
        i=0
        cityTmpArr=[]
        while i < len(multi_cities):
                yr_int=int(years[count])
                yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
                #yrCityVar = pylab.var(yrTempArr)
                cityTmpArr.append(yrTempArr)
                #print(cityTmpArr)
                i +=1
        #print('cityTmpArr =',cityTmpArr, '\n')
        #citiesAvgVar = pylab.mean(cityTmpArr)
        
        #stDeviation = math.sqrt(citiesAvgVar)
        #print('Cities Avg =',citiesAvg,'\n')
        #tempCitiesArr.append(cityTmpArr)
        stDeviation = pylab.std(cityTmpArr)
        #stDeviation=math.sqrt(stDeviation)
        stdCitiesArr.append(stDeviation)
        count +=1
    print('i =',i)
    print('count =',count,'\n')
    #print(cityTmpArr)
    return stdCitiesArr
    # TODO
    tempCitiesArr=[]
    count=0
    while count < len(years):
        i=0
        cityTmpArr=[]
        while i < len(multi_cities):
            yr_int = int(years[count])
            yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
            yrAvg=float(pylab.mean(yrTempArr))
            yrCityVarArr = pylab.square(yrTempArr-yrAvg) 
            cityTmpArr.append(yrCityVarArr)
            i +=1
        #print(cityTmpArr)
        variance = pylab.sum(cityTmpArr)/((len(multi_cities)*365))
        stDeviation = pylab.sqrt(variance) 
        tempCitiesArr.append(stDeviation)
        
        count +=1

    return tempCitiesArr
"""
def evaluate_models_on_testing(x, y, models):
    """
    For each regression model, compute the RMSE for this model and plot the
    test data along with the model’s estimation.

    For the plots, you should plot data points (x,y) as blue dots and your best
    fit curve (aka model) as a red solid line. You should also label the axes
    of this figure appropriately and have a title reporting the following
    information:
        degree of your regression model,
        RMSE of your model evaluated on the given data points. 

    Args:
        x: an 1-d pylab array with length N, representing the x-coordinates of
            the N sample points
        y: an 1-d pylab array with length N, representing the y-coordinates of
            the N sample points
        models: a list containing the regression models you want to apply to
            your data. Each model is a pylab array storing the coefficients of
            a polynomial.

    Returns:
        None
    """
    # TODO
    i=0
    modNumber = len(models)
    print('plot models ',modNumber,'on Testing datas')
    while i < (modNumber):
        #string_rsquare ='rsquare'
        plt.xlabel(' year ')
        plt.ylabel('degrees Celsius')
        plt.plot(x,y, 'bo')
        estimated = np.array(pylab.polyval(models[i],x))
        plt.plot(x,estimated, 'r--', label =' model')
        string_degree =str(len(models[i])-1)
        string_rmse = str(rmse(y, estimated))
        plt.title('Degree:' + string_degree+ '\n  RMSE:' + string_rmse)
        plt.show()
        i += 1

if __name__ == '__main__':

    pass 

    # Part A.4
    # TODO: replace this line with your code

"""
resultList = generate_models(pylab.array([1961,1962,1963]), pylab.array([-4.4,-5.5,-6.6]), [1,2])
print(resultList)
out1Polyfit = pylab.polyfit(pylab.array([1961,1962,1963]), pylab.array([-4.4,-5.5,-6.6]),1)
print(out1Polyfit)  
out2Polyfit = pylab.polyfit(pylab.array([1961,1962,1963]), pylab.array([-4.4,-5.5,-6.6]),2)
print(out2Polyfit) 

x = pylab.array(range(50))
y = pylab.array(range(50))
degrees = [1]
models = generate_models(x, y, degrees)
print("models =", models)

degrees = [1,2,20]
models = ps5.generate_models(x, y, degrees)
print("complex models =",models)
    # Part B
    # TODO: replace this line with your code
y = pylab.array(range(10))
est = pylab.array([5.001]*10)
r_sq = r_squared(y, est)
print(r_sq)
    # Part C
    # TODO: replace this line with your code

    # Part D.2
    # TODO: replace this line with your code

    # Part E
    # TODO: replace this line with your code
"""
x = pylab.array(range(50))
y = pylab.array(range(50))
degrees = [1]
models = generate_models(x, y, degrees)
evaluate_models_on_training(x, y, models)

y = pylab.array(range(0,100,2))
degrees = [1, 2]
models = generate_models(x, y, degrees)
evaluate_models_on_training(x, y, models)


y_list=(1,2,3,4,5,50,40,30,20,10,1,2,3,4,5,50,40,30,20,10,1,2,3,4,5,50,40,30,20,10,1,2,3,4,5,50,40,30,20,10,1,2,3,4,5,50,40,30,20,10)
y=pylab.array(y_list)
degrees = [1,2,20]
models = generate_models(x, y, degrees)
evaluate_models_on_training(x, y, models)


"""
Problem A41` analyze Jan10 temp from 61 to 2009
"""
usTemp = Climate('data.csv')
yr =1961
yrArrX =[]
tempArrY = []
while yr <= 2009:
    yrArrX.append(int(yr))
    tempArrY.append(usTemp.get_daily_temp('NEW YORK',1,10,yr))
    yr +=1


tempArrY = pylab.array(tempArrY)
plt.plot(yrArrX,tempArrY, 'bs')
p41models = generate_models(yrArrX, tempArrY, [1])
evaluate_models_on_training(yrArrX, tempArrY, p41models)
"""
Problem A42` analyze average annual temp from 61 to 2009
"""
yrArrX =[]
tempArrY = []
yr =1961
while yr <= 2009:
    yrArrX.append(int(yr))
    yrTempArr=usTemp.get_yearly_temp('NEW YORK',yr)
    yrAvg = pylab.mean(yrTempArr)
    tempArrY.append(yrAvg)
    #tempArrY.append(usTemp.get_daily_temp('NEW YORK',1,10,yr)) ##test
    yr +=1


tempArrY = pylab.array(tempArrY)
plt.plot(yrArrX,tempArrY, 'bs')
p42models = generate_models(yrArrX, tempArrY, [1])
evaluate_models_on_training(yrArrX, tempArrY, p42models)
evaluate_models_on_testing(yrArrX, tempArrY, p42models)

##
climate = Climate('data.csv')
test_years = pylab.array(TESTING_INTERVAL)
result = gen_std_devs(climate, ['SEATTLE'], [1961,1962])
print('seattle only',result)

result = gen_std_devs(climate, ['SEATTLE','SEATTLE'], [1961,1962])
print('seattle seattle =',result)
"""
y = [1, 2, 3, 4, 5, 6, 7, 8, 9]
estimate = [1, 4, 9, 16, 25, 36, 49, 64, 81]
result = rmse(pylab.array(y), pylab.array(estimate))
print(result)
"""

# test std
climate = Climate('data.csv')
years = pylab.array(TRAINING_INTERVAL)
result = gen_std_devs(climate, CITIES, [1961,1962])
print(result)
result = gen_std_devs(climate, ['TAMPA', 'DALLAS'],[1961,1962])
print(result)

"""
yr_int=1961
multi_cities=['SEATTLE', 'SEATTLE']
i=0
stdCitiesArr=[]
yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
                #yrCityVar = pylab.var(yrTempArr)
cityTmpArr.append(yrTempArr)
i=1
yrTempArr=climate.get_yearly_temp(multi_cities[i],yr_int)
                #yrCityVar = pylab.var(yrTempArr)
cityTmpArr.append(yrTempArr)
stDeviation = pylab.var(cityTmpArr)
stDeviation = math.sqrt(stDeviation)
print('stDev for 2cities =',stDeviation)

"""
"""
result = gen_std_devs(climate, ['TAMPA', 'TAMPA'],[1961,1962])
print(result)
result = gen_std_devs(climate, ['DALLAS', 'DALLAS'],[1961,1962])
print(result)
"""

"""
result = gen_cities_avg(climate, ['TAMPA', 'DALLAS'], [2010,2011,2012,2013])
print(result)

result = gen_cities_avg(climate, ['TAMPA','TAMPA','TAMPA'], [2010,2011,2012,2013])
print(result)

result = gen_cities_avg(climate, ['DALLAS','DALLAS','DALLAS'], [2010,2011,2012,2013])
print(result)

"""