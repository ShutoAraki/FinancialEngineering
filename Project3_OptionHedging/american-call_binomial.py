"""

Financial Engineering Project 3: Option Hedging
    This program calculates the American Call premium using Binomial Tree Pricing Model
    It also compares the convergence in the Binomial Tree Model to the Black Scholes
    Model.

@authors: Muhammad Shahraiz Niazi and Shuto Araki
@professor: Dr. Zhixin Wu
@date: 05/12/2018

========== GLOBAL VARIABLES ==========
** Dictionary is a data structure that connects key and value pair.
   All dictionaries in this project contains dates as keys. **

call_prices:
    A dictionary of call prices

sigma_list:
    A dictionary of implied volatilities

stock_prices:
    A dictionary of stock prices
    
"""

# Import Dependencies
import numpy as np
from scipy.stats import norm
import math
import matplotlib.pyplot as plt


def binomial_tree_call(N = 5, T = 33/252, S0 = 52.39, sigma = 0.3746345, r = 0.0218, K = 52.5, q = 0.0225):
    """ 
    This function displays two binomial trees: a stock price movement and calculated
    option value at each node. At the end, it also displays the final value of 
    option and delta. By default, all the arguments are assigned from the data
    at the beginning of the hedging period.
    
    Args: 
        N: The number of steps in the binomial tree
        T: Time to maturity (annualized)
        S0: Initial stock price
        sigma: Volatility (annualized)
        r: Risk-free interest rate
        K: Strike price of the call option
        q: Dividend yield rate
    Returns:
        Nothing
    """
    
    #initialize
    dt = T/N
    u = np.exp(sigma*np.sqrt(dt))
    d = 1/u
    p = (np.exp((r-q)*dt)-d)/(u-d)
    
    # Price Tree
    price_tree = np.zeros([N+1,N+1])
    #Option Tree
    option = np.zeros([N+1,N+1])
    
    for i in range(N+1):
        for j in range(i+1):
            price_tree[j,i] = S0*(d**j)*(u**(i-j))

    
    #Option values
    option = np.zeros([N+1,N+1])
    option[:,N] = np.maximum(np.zeros(N+1), price_tree[:,N]-K)
    for i in np.arange(N-1,-1,-1):
        for j in np.arange(0,i+1):
            option[j,i] =np.maximum(np.exp(-r*dt)*(p*option[j,i+1]+(1-p)*option[j+1,i+1]), price_tree[j,i]-K)
        
        
    for i in range(N+1):
        print("At t =", round(i*dt, 3))
        for j in range(i+1):
            print(end=' ')
            if(price_tree[j,i]!=0):
                print(round(price_tree[j,i], 2), end='  ',)
        print()
        for j in  range(i+1):
                print("({0:.2f})".format(option[j,i]), end='  ')                
        print('\n')
        
    
    print("The final value of option is at t = 0:", round(option[0,0], 2))
    delta = (option[0,1] - option[1,1]) / (price_tree[0,1] - price_tree[1,1])
    print("Delta:", round(delta, 5))
    
    
def call_option_price(N = 200, T = 33/252, S0 = 52.39, sigma = 0.3746345, r = 0.0218, K = 52.5, q = 0.0225, delta=False):
    """ 
    This function returns the call option premium. If specified, it returns
    delta.
    
    Args: 
        N: The number of steps in the binomial tree
        T: Time to maturity (annualized)
        S0: Initial stock price
        sigma: Volatility (annualized)
        r: Risk-free interest rate
        K: Strike price of the call option
        q: Dividend yield rate
        delta: If True, the function returns the delta value.
        
    Returns:
        float: option at t = 0 if delta is False
        float: delta value at t = 0 if delta is True
    """
    #Dividend yield rate subtraction
    
    # initialize
    dt = T/N
    u = np.exp(sigma*np.sqrt(dt))
    d = 1/u
    p = (np.exp((r-q)*dt)-d)/(u-d)
    
    # Price Tree
    price_tree = np.zeros([N+1,N+1])
    
    for i in range(N+1):
        for j in range(i+1):
            price_tree[j,i] = S0*(d**j)*(u**(i-j))   

    #Option values
    option = np.zeros([N+1,N+1])
    option[:,N] = np.maximum(np.zeros(N+1), price_tree[:,N]-K)
    
    for i in np.arange(N-1,-1,-1):
        for j in np.arange(0,i+1):
            option[j,i] =np.maximum(np.exp(-r*dt)*(p*option[j,i+1]+(1-p)*option[j+1,i+1]), price_tree[j,i]-K)
    
    if delta:
        return (option[0,1] - option[1,1]) / (price_tree[0,1] - price_tree[1,1])
    
    return option[0,0]
    

def approxBlackScholes(start = 3, end = 1000, step = 50):
    """
    This function displays how the Binomial Tree Model converges to a certain
    value as the number of steps in the tree increases.
    
    Args:
        start: starting number of steps in int
        end: ending number of steps in int
        step: how many steps it skips from one calculation to another
        
    Returns:
        Nothing
    """
    
    for n in range(start, end, step):
        print("N =", n, ":", call_option_price(N = n))
        
        
def exactBlackScholes(T = 33/252, S0 = 52.39, sigma = 0.3746345, r = 0.0218, K = 52.5, q = 0.0225):
    """
    This function calculates the exact value of call option premium assuming
    that it is an European option.
    
    Args:
        T: Time to maturity (annualized)
        S0: Initial stock price
        sigma: Volatility (annualized)
        r: Risk-free interest rate
        K: Strike price of the call option
        q: Dividend yield rate
    
    Returns:
        Tuple of floats: call and put option premium at t = 0
    """
    d1 = (math.log(S0 / K) + (r - q + sigma**2 / 2) * T) / (sigma * math.sqrt(T))
    d2 = d1 - sigma*math.sqrt(T)
    
    e = math.e
    c = S0 * e**(-q*T) * norm.cdf(d1) - K * e**(-r*T) * norm.cdf(d2)
    p = K * e**(-r*T) * norm.cdf(-d2) - S0 * e**(-q*T) * norm.cdf(-d1)
    
    return c, p


def displayBlackScholes(T = 33/252, S0 = 52.39, sigma = 0.3746345, r = 0.0218, K = 52.5, q=0):
    """
    This function calculates the call and put option premium at t = 0 and 
    displays the results.
    
    Args:
        T: Time to maturity (annualized)
        S0: Initial stock price
        sigma: Volatility (annualized)
        r: Risk-free interest rate
        K: Strike price of the call option
        q: Dividend yield rate
    
    Returns: 
        Nothing
    """
    c, p = exactBlackScholes(T, S0, sigma, r, K, q)
    
    print("c =", c)
    print("p =", p)


def plotOptionPrices(start = 1, end = 200, step = 1):
    """
    This function displays a graph that shows how the Binomial Tree Model
    converges to the Black-Scholes Model as the number of steps in the tree
    increases.
    
    Args:
        start: starting number of steps in int
        end: ending number of steps in int
        step: how many steps it skips from one calculation to another
        
    Returns:
        Nothing
    """
    callPriceList = []
    c, p = exactBlackScholes()
    
    for n in range(start, end, step):
        callPriceList.append(call_option_price(N = n))
    
    plt.plot(list(range(start, end, step)), callPriceList, label = "Binomial Tree")
    plt.plot([start, end], [c, c], '--', color='r', label = "Black Scholes")
    plt.xlabel("Number of Steps in Binomial Tree")
    plt.ylabel("Call Option Price")
    plt.title("The Binomial Tree Model approximates the Black Scholes Model")
    plt.legend(loc="upper right", shadow=True, fancybox=True)
    plt.show()
    

# Call Option Prices During the Hedging Period
# INTC call maturity on 06/15/2018
call_prices = {"05/01/2018": 2.08, 
               "05/02/2018": 2.13, 
               "05/03/2018": 1.80, 
               "05/04/2018": 2.08, 
               "05/07/2018": 2.45,
               "05/08/2018": 2.42, 
               "05/09/2018": 2.81, 
               "05/10/2018": 3.06, 
               "05/11/2018": 3.04
              }

# Implied Volatility During the Hedging Period
sigma_list = {  "05/01/2018": 0.2832, 
                "05/02/2018": 0.2838, 
                "05/03/2018": 0.2821, 
                "05/04/2018": 0.2796, 
                "05/07/2018": 0.2759,
                "05/08/2018": 0.2972, 
                "05/09/2018": 0.2741, 
                "05/10/2018": 0.2801, 
                "05/11/2018": 0.2700
              }

# Stock Prices During the Hedging Period
stock_prices = { "05/01/2018": 52.39, 
                 "05/02/2018": 52.54, 
                 "05/03/2018": 51.97, 
                 "05/04/2018": 52.63, 
                 "05/07/2018": 53.40,
                 "05/08/2018": 53.15, 
                 "05/09/2018": 54.11, 
                 "05/10/2018": 54.48, 
                 "05/11/2018": 54.59
               }    

def dynamicDeltaHedgingSimulation(historicalSigma = False):
    """
    This function simulates the dynamic delta hedging using INTC (Intel Corp.)
    stocks and calls. The hedging period was 05/01/2018 to 05/11/2018.
    The position was changed every business day.
    
    Args:
        historicalSigma: boolean value that chooses whether to use historical
                         volatility or implied volatility
                         
    Returns:
        Nothing
    """
    # Replace the implied volatility values with historical ones.
    if historicalSigma:
        print("USING HISTORICAL VOLATILITY\n")
        # A list of delta values
        delta_list = [
            call_option_price(N=33, T=33/252, S0=stock_prices["05/01/2018"], sigma=0.3746345, delta=True), 
            call_option_price(N=33, T=32/252, S0=stock_prices["05/02/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=31/252, S0=stock_prices["05/03/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=30/252, S0=stock_prices["05/04/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=29/252, S0=stock_prices["05/07/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=28/252, S0=stock_prices["05/08/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=27/252, S0=stock_prices["05/09/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=26/252, S0=stock_prices["05/10/2018"], sigma=0.3746345, delta=True),
            call_option_price(N=33, T=25/252, S0=stock_prices["05/11/2018"], sigma=0.3746345, delta=True)    
                     ]
    else:
        print("USING IMPLIED VOLATILITY\n")
        delta_list = [
            call_option_price(N=33, T=33/252, S0=stock_prices["05/01/2018"], sigma=sigma_list["05/01/2018"], delta=True), 
            call_option_price(N=33, T=32/252, S0=stock_prices["05/02/2018"], sigma=sigma_list["05/02/2018"], delta=True),
            call_option_price(N=33, T=31/252, S0=stock_prices["05/03/2018"], sigma=sigma_list["05/03/2018"], delta=True),
            call_option_price(N=33, T=30/252, S0=stock_prices["05/04/2018"], sigma=sigma_list["05/04/2018"], delta=True),
            call_option_price(N=33, T=29/252, S0=stock_prices["05/07/2018"], sigma=sigma_list["05/07/2018"], delta=True),
            call_option_price(N=33, T=28/252, S0=stock_prices["05/08/2018"], sigma=sigma_list["05/08/2018"], delta=True),
            call_option_price(N=33, T=27/252, S0=stock_prices["05/09/2018"], sigma=sigma_list["05/09/2018"], delta=True),
            call_option_price(N=33, T=26/252, S0=stock_prices["05/10/2018"], sigma=sigma_list["05/10/2018"], delta=True),
            call_option_price(N=33, T=25/252, S0=stock_prices["05/11/2018"], sigma=sigma_list["05/11/2018"], delta=True)    
                     ]
    
    portfolio = 100000
    
    print("Stock: INTC")
    print("Initial Porfolio Value:", portfolio)
    print()
    
    print("================================")
    print("05/01/2018")
    print("\tPortfolio Value:", portfolio)
    print("\tShort 5 Calls")
    print("\tDelta =", delta_list[0])
    numOfStock = round(delta_list[0] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/02/2018")
    portfolio = 100000 + (stock_prices["05/02/2018"] - stock_prices["05/01/2018"]) * numOfStock - (call_prices["05/02/2018"] - call_prices["05/01/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[1])
    numOfStock = round(delta_list[1] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/03/2018")
    portfolio = 100000 + (stock_prices["05/03/2018"] - stock_prices["05/02/2018"]) * numOfStock - (call_prices["05/03/2018"] - call_prices["05/02/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[2])
    numOfStock = round(delta_list[2] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/04/2018")
    portfolio = 100000 + (stock_prices["05/04/2018"] - stock_prices["05/03/2018"]) * numOfStock - (call_prices["05/04/2018"] - call_prices["05/03/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[3])
    numOfStock = round(delta_list[3] * 500)
    print("\tHold", numOfStock, "stocks")
    print()

    print("================================")
    print("05/07/2018")
    portfolio = 100000 + (stock_prices["05/07/2018"] - stock_prices["05/04/2018"]) * numOfStock - (call_prices["05/07/2018"] - call_prices["05/04/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[4])
    numOfStock = round(delta_list[4] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/08/2018")
    portfolio = 100000 + (stock_prices["05/08/2018"] - stock_prices["05/07/2018"]) * numOfStock - (call_prices["05/08/2018"] - call_prices["05/07/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[5])
    numOfStock = round(delta_list[5] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/09/2018")
    portfolio = 100000 + (stock_prices["05/09/2018"] - stock_prices["05/08/2018"]) * numOfStock - (call_prices["05/09/2018"] - call_prices["05/08/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[6])
    numOfStock = round(delta_list[6] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/10/2018")
    portfolio = 100000 + (stock_prices["05/10/2018"] - stock_prices["05/09/2018"]) * numOfStock - (call_prices["05/10/2018"] - call_prices["05/09/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[7])
    numOfStock = round(delta_list[7] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/11/2018")
    portfolio = 100000 + (stock_prices["05/11/2018"] - stock_prices["05/10/2018"]) * numOfStock - (call_prices["05/11/2018"] - call_prices["05/10/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[8])
    numOfStock = round(delta_list[8] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("IF NOT HEDGED, portfolio value would have been", 100000 - (call_prices["05/11/2018"] - call_prices["05/01/2018"]) * 500)


def staticDeltaHedgingSimulation(historicalSigma=False):
    """
    This function simulates the static delta hedging using INTC (Intel Corp.)
    stocks and calls. The hedging period was 05/01/2018 to 05/11/2018.
    The position was changed once at the beginning.
    """
    
    # Replace the implied volatility values with historical ones.
    if historicalSigma:
        print("USING HISTORICAL VOLATILITY\n")
        # A list of delta values
        delta_list = [
            call_option_price(N=33, T=33/252, S0=stock_prices["05/01/2018"], sigma=0.3746345, delta=True), 
            call_option_price(N=33, T=25/252, S0=stock_prices["05/11/2018"], sigma=0.3746345, delta=True)    
                     ]
    else:
        print("USING IMPLIED VOLATILITY\n")
        delta_list = [
            call_option_price(N=33, T=33/252, S0=stock_prices["05/01/2018"], sigma=sigma_list["05/01/2018"], delta=True), 
            call_option_price(N=33, T=25/252, S0=stock_prices["05/11/2018"], sigma=sigma_list["05/11/2018"], delta=True)    
                     ]
    
    portfolio = 100000
    print("Stock: INTC")
    print("Initial Porfolio Value:", portfolio)
    print()
    
    print("================================")
    print("05/01/2018")
    print("\tPortfolio Value:", portfolio)
    print("\tShort 5 Calls")
    print("\tDelta =", delta_list[0])
    numOfStock = round(delta_list[0] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("================================")
    print("05/11/2018")
    portfolio = 100000 + (stock_prices["05/11/2018"] - stock_prices["05/01/2018"]) * numOfStock - (call_prices["05/11/2018"] - call_prices["05/01/2018"]) * 500
    print("\tPortfolio Value:", portfolio)
    print("\tDelta =", delta_list[1])
    numOfStock = round(delta_list[1] * 500)
    print("\tHold", numOfStock, "stocks")
    print()
    
    print("IF NOT HEDGED, portfolio value would have been", 100000 - (call_prices["05/11/2018"] - call_prices["05/01/2018"]) * 500)

