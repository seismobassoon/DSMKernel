import math
import time
import webbrowser

"""
Modified on Fri Feb  3 16:35:46 2023

From Helle Pedersen lecture at Université Grenoble Alpes
Modification added by Lorraine Delaroque

"""

try:
    import folium
except ModuleNotFoundError:
    print("\nfolium package required!")
    raise

try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    print("\nmatplotlib package required!")
    raise

try:
    from obspy import Stream
    from obspy.geodetics import gps2dist_azimuth
    from scipy.signal import periodogram
except ModuleNotFoundError:
    print("\nobspy package required!")
    raise




def get_depth_color(depth):
    colors = [
        "#FF0000",
        "#FF3300",
        "#FF6600",
        "#FFCC00",
        "#CCFF00",
        "#66FF00",
        "#00FF00",
    ]
    # <=20000   <=10000   <=5000    <=1000    <=500     <=100     >100
    limits = [20000, 10000, 5000, 1000, 500, 100]
    for i, val in enumerate(limits):
        if depth and depth >= val:
            return colors[i]
    return colors[-1]



def plot_traces(traces):

    fig = plt.figure(figsize=(8, 6))
    for n, tr in enumerate(traces):
        ax = fig.add_subplot(2, 1, n + 1)
        ax.plot(tr.times("matplotlib"), tr.data, "-b")
        ax.xaxis_date()  # treat the x data as dates
        fig.autofmt_xdate()  # avoid date ticklabels overlap

    fig.tight_layout()  # subplots are nicely fit in the figure
    plt.show()


def plot_record_section(
    st, stations, eq_lat, eq_lon, size=(1200, 1000), show=True, outfile=None
):

    if not st or not stations:
        print("Station or stream empty.")
        return

    st2 = Stream()
    for tr in st:
        for net, sta, lat, lon, _ in stations:
            # We keep traces with a corresponding station only:
            if tr.stats.network == net and tr.stats.station == sta:
                tr.stats.coordinates = {"latitude": lat, "longitude": lon}
                # Work out the distances from the latitude and longitude:
                tr.stats.distance = gps2dist_azimuth(lat, lon, eq_lat, eq_lon)[0]
                st2.append(tr)

    # Plot the section:
    figure = plt.figure(figsize=(size[0] // 100, size[1] // 100))
    if len(st2) < 2:
        print("Cannot build plot section with less than two traces.\n")
        return

    begin = min(tr.stats.starttime for tr in st2)
    st2.trim(starttime=begin, pad=True, fill_value=0)

    st2.plot(type="section", linewidth=0.25, grid_linewidth=0.25, fig=figure)
    ax = figure.axes[0]

    ds = [(tr.stats.distance, tr.stats.station) for tr in st2]
    ds.sort()
    for n, (dist, sta) in enumerate(ds):
        # to avoid merged titles
        ycoord = 1.05 if (n + 1) % 2 == 0 else 1.07
        ax.text(dist / 1e3, ycoord * ax.get_ylim()[1], sta, fontsize=7, rotation=45)

    if outfile:
        plt.savefig(outfile)
    elif show:
        plt.show()
    return figure


def get_periodogram(x, fs=1, semilog=True, decibel=True, show=True, outfile=None):
    """Estimate power spectral density of a timeserie using a periodogram.

    :param x: timeserie values
    :returns: values in V**2/Hz if x is measured in V and fs is measured in Hz

    """
    freqs, psd = periodogram(
        x,
        fs=fs,  # Sampling frequency of the x timeseries
        window="boxcar",
        nfft=None,
        detrend="constant",
        return_onesided=True,
        scaling="density",  # where Pxx has units of V**2/Hz
    )

    ylabel = "Power"
    if decibel:
        psd = [10 * math.log10(n / psd[0]) for n in psd]  # in dB
        ylabel = "Power (dB)"

    plt.figure(figsize=(8, 6), dpi=100)
    if semilog:
        plt.semilogx(freqs, psd)
    else:
        plt.plot(freqs, psd)
    plt.title("PSD: power spectral density")
    plt.xlabel("Frequency (Hz)")
    plt.ylabel(ylabel)
    plt.tight_layout()

    if outfile:
        plt.savefig(outfile)
    elif show:
        plt.show()

    return (freqs, psd)


