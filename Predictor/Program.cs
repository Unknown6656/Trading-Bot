using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Microsoft.ML;

namespace Predictor
{
    public static class Program
    {
        public const string PATH = @"data/apple-2017.csv";


        public static void Main(string[] args)
        {
            SamplePoint[] csv = (from line in File.ReadAllLines(PATH).Skip(1)
                                 let tokens = line.Split(',')
                                 let date = DateTime.ParseExact(tokens[0], "d-MMM-y", null)
                                 let v_op = double.Parse(tokens[1])
                                 let v_cl = double.Parse(tokens[2])
                                 let v_lo = double.Parse(tokens[3])
                                 let v_hi = double.Parse(tokens[4])
                                 orderby date ascending
                                 select new SamplePoint(date, v_op, v_cl, v_lo, v_hi)).ToArray();
            DateTime min = csv[0].Date;
            (double X, double Y)[] points = csv.Select(sp => (sp.Date.Subtract(min).TotalDays, sp.Average)).ToArray();






            List<(double r2, double a, double b, double next)> predictions = new List<(double, double, double, double)>();
            int len = points.Length;

            double n = 0;
            double c = 0;

            for (int i = 10; i < len; ++i)
            {
                (double X, double Y)[] data = points[(len - 1 - i)..];

                LinearRegression(data, out double r2, out double b, out double a);

                double next = data.Length * a + b;

                predictions.Add((r2, b, a, next));

                n += next * i;
                c += i;
            }

            double lol = n / c;



        }


        public static void LinearRegression((double X, double Y)[] data, out double r2, out double b, out double a)
        {
            int count = data.Length;
            double Σx = 0;
            double Σy = 0;
            double Σx2 = 0;
            double Σy2 = 0;
            double Σco = 0;

            for (int i = 0; i < count; i++)
            {
                (double x, double y) = data[i];

                Σco += x * y;
                Σx += x;
                Σy += y;
                Σx2 += x * x;
                Σy2 += y * y;
            }

            double ssX = Σx2 - (Σx * Σx / count);
            double ssY = Σy2 - (Σy * Σy / count);

            double rNumerator = (count * Σco) - (Σx * Σy);
            double rDenom = (count * Σx2 - (Σx * Σx)) * (count * Σy2 - (Σy * Σy));
            double sCo = Σco - ((Σx * Σy) / count);

            double meanX = Σx / count;
            double meanY = Σy / count;
            double dblR = rNumerator / Math.Sqrt(rDenom);

            r2 = dblR * dblR;
            b = meanY - (sCo / ssX * meanX);
            a = sCo / ssX;
        }
    }

    public sealed class SamplePoint
    {
        public DateTime Date { get; }
        public double Open { get; }
        public double Close { get; }
        public double Low { get; }
        public double High { get; }
        public double Average { get; }


        public SamplePoint(DateTime date, double open, double close, double low, double high)
        {
            Date = date;
            Open = open;
            Close = close;
            Low = low;
            High = high;
            Average = (open + close + low + high) / 4;
        }

        public override int GetHashCode() => HashCode.Combine(Date, Open, Close, Low, High);
    }
}
