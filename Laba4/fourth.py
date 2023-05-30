from pandas import *
df = read_csv("D:/traffic_security.csv")
df1 = df.iloc[::,1]
b = []
for i in range(len(df1)):
  t = df1[i]
  r = list(t)
  v = []
  for j in r:
    if j != ".":
      v.append(j)
    else:
      break
  z = "".join(v)
  b.append(z)
  v.clear()


df2 = DataFrame({"first_num_ip":b})
df3 = df2[df2["first_num_ip"].isin(["12","13","14"])]
df4 = []
for i in df3.index:
    df4.append(i)

df5 = df.loc[df4]
df6 = df5.loc[df5.iloc[::,4] == df5.iloc[::,4].min()]
df7 = list(df6.iloc[::,3])
df8 = set(df7)
df9 = list(df8)
df10 = []
for i in df9:
    x = df7.count(i)
    df10.append(x)
df11 = DataFrame({"first_num_port":df9,
                  "counts":df10})
df12 = df11[df11["counts"] == df11["counts"].max()]
df13 = df6[df6.iloc[::,3] == list(df12["first_num_port"])[0]]
df14 = list(df13.iloc[::,3])[0]
print(df14)