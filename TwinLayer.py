import re
import datetime
import folium
from folium.plugins import MarkerCluster
import branca.colormap as cm
def find(lst, target):
    result = []
    for i,x in enumerate(lst):
        if  float(x) == float(target):
            result.append(i)
    return result
def baseLayer(__mapit, __IDlatlon, __person, __subsetIndex, __twinIndex, __SVID):
    # __subsetIndex to test example maps (use 1 for plotting all points)
    # twin A - navy (0,0,128)
    # twin B - fuschia （255，0，255）

    if __twinIndex == 1:
        strColor = "navy"
        RBGColor = (0, 0, 128)
    elif __twinIndex == 2:
        strColor = 'fuchsia'
        RBGColor = (255, 0, 255)
    testIn = __person[0::__subsetIndex]
    marker_cluster = MarkerCluster(control=False, icon_create_function="""
    function(cluster) {
        return L.divIcon({
        html: '<b>' + cluster.getChildCount() + '</b>',
        className: "leaflet-div-icon"
        });
        }
    """)
    __mapit.add_child(marker_cluster)

    g1 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'School Weekday Daytime')
    __mapit.add_child(g1)
    g2 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'School Weekday Evening')
    __mapit.add_child(g2)
    g3 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'School Weekend Daytime')
    __mapit.add_child(g3)
    g4 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'School Weekend Evening')
    __mapit.add_child(g4)
    g5 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'Summer Weekday Daytime')
    __mapit.add_child(g5)
    g6 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'Summer Weekday Evening')
    __mapit.add_child(g6)
    g7 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'Summer Weekend Daytime')
    __mapit.add_child(g7)
    g8 = folium.plugins.FeatureGroupSubGroup(marker_cluster, 'Summer Weekend Evening')
    __mapit.add_child(g8)

    adjust = len(testIn)  # use to change opacity of circle Markers
    __dateTime = []
    for i in range(0, len(testIn)):
        x = testIn[i]
        criteria = re.split('-| |:', __IDlatlon[x][3])

        d = []
        for m in criteria:
            d += [int(m)]

        criteriaNew = datetime.datetime(d[0], d[1], d[2], d[3], d[4], d[5])
        __dateTime += [criteriaNew]
        # 5:00 pm daytime / evening
        # June 1 - August 31 summer / school
        # weekend / weekday (including Fridays)
        # check if weekday is 1..5 [1,6)
        # check if month is 6..8 [6,9)
        # check if time is in 7..17 [7,18)

        # plot markers
        if ((criteriaNew.month in range(6, 9)) == False and
                (criteriaNew.isoweekday() in range(1, 6)) == True and
                (criteriaNew.hour in range(7, 18)) == True):
            g1.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == False and
                (criteriaNew.isoweekday() in range(1, 6)) == True and
                (criteriaNew.hour in range(7, 18)) == False):
            g2.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == False and
                (criteriaNew.isoweekday() in range(1, 6)) == False and
                (criteriaNew.hour in range(7, 18)) == True):
            g3.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == False and
                (criteriaNew.isoweekday() in range(1, 6)) == False and
                (criteriaNew.hour in range(7, 18)) == False):
            g4.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == True and
                (criteriaNew.isoweekday() in range(1, 6)) == True and
                (criteriaNew.hour in range(7, 18)) == True):
            g5.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == True and
                (criteriaNew.isoweekday() in range(1, 6)) == True and
                (criteriaNew.hour in range(7, 18)) == False):
            g6.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == True and
                (criteriaNew.isoweekday() in range(1, 6)) == False and
                (criteriaNew.hour in range(7, 18)) == True):
            g7.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))
        if ((criteriaNew.month in range(6, 9)) == True and
                (criteriaNew.isoweekday() in range(1, 6)) == True and
                (criteriaNew.hour in range(7, 18)) == False):
            g8.add_child(
                folium.CircleMarker(location=[__IDlatlon[x][1], __IDlatlon[x][2]], fill=True, fill_color=strColor,
                                    color="gray", fill_opacity=i / adjust))

    colors = [(255, 255, 255), RBGColor]
    colormap = cm.LinearColormap(colors=colors).to_step(100)
    headTime = __dateTime[0]
    endTime = __dateTime[-1]
    colormap.caption = "Twin A_{whichTwin} {whatDevice}: {left_aligned} to {right_aligned}".format(
        whichTwin =__SVID,
        whatDevice = __IDlatlon[__person[0]][5],
        left_aligned=headTime,
        right_aligned=endTime)
    __mapit.add_child(colormap)

    return __mapit
