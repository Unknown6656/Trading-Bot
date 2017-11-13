#define USE_CSV

using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Drawing.Text;
using System.Diagnostics;
using System.Management;
using System.Drawing;
using System.Linq;
using System.IO;
using System;

using Microsoft.FSharp.Core;

using static TradingBot.Simulator;
using static TradingBot.Bot;
using static System.Math;

namespace BotVisualizer
{
    using static Program;


    public static class Program
    {
        internal static IBot BOT;
        internal const string target = @"./bot.png";
        internal const int BORDER = 50;
        internal const int BORDER_EX_RIGHT = 900;
        internal const int HEIGHT = 700;
        internal const double FAC_WIDTH = 4.0;
        internal const float FNT_HEIGHT = 13;
        internal const uint CLR_BACKGROUND = 0xff222222;
        internal const uint CLR_VOLUME_BOT = 0xff880000u;
        internal const uint CLR_VOLUME = 0xffcc0000u;
        internal const uint CLR_RATE = 0xff50ff50u;
        internal const uint CLR_BUDGET = 0xff5080ffu;
        internal const uint CLR_BUDGET_SHARES = 0xff0040a0u;
        internal const uint CLR_BUDGET_DIRECT = 0xff904090u;
        internal const uint CLR_TEXT = 0xffccccccu;
        internal const uint CLR_TEXT_PALE = 0xff383838u;
        internal const char CURR = '€';
        internal const bool PAUSE = false;
        internal const string SRC = "data/nasdaq-2017.csv";
        internal const string TARGET = "bot.png";
        internal const double BUDGET = 1000;
        internal const ulong LOOKBACK = 5;
        internal const double RISK = .1;
        private static Func<double, double> __fptr;

#pragma warning disable CS0168, CS0219
        private static ((ulong timestamp, DataEx data, State state)[] data, string source) Run()
        {
            BOT = new Bot_01();

            string path = SRC;
            double max = 600;
            var sim = new Func<double, double>[]
            {
                x =>
                {
                    x /= 10;

                    return Sin(x) / 2 + 100 + Cos(4 * x) / 4 + Sin(3 * Sin(x)) / 8 - 10 * Atan(x / 20 - 30);
                },
                x =>
                {
                    double xc = x;

                    x /= max;

                    if ((x > .4) && (x < .5))
                        x = .9 - x;

                    x += Sin(xc / 8) / 4;
                    x *= 10;
                    x += 20;

                    return x;
                },
                x => Sin(x / 10) * 10 + 50,
            };

            // https://stooq.com/db/h/
            // https://www.google.com/finance/historical?output=csv&q= ...
#if USE_CSV
            var result = SimulateCSV(BOT, path, RISK, BUDGET, LOOKBACK);
            var fi = new FileInfo(path);

            path += $": {fi.Length / 1024d:F1}KB   created: {fi.CreationTime}   changed: {fi.LastWriteTime}";
#else
            __fptr = sim[1];

            var result = SimulateFake(BOT, new AbstractRate(new Converter<double, double>(__fptr), max), RISK, BUDGET, LOOKBACK);
            var body = __fptr.Method.GetMethodBody();

            path = $"{body.LocalSignatureMetadataToken:x}:{body.MaxStackSize:x}@{__fptr.GetType().FullName}";
#endif
            return ((from t in result select (t.Item2.Item1, t.Item1, t.Item2.Item2)).ToArray(), path);
        }
#pragma warning restore CS0168, CS0219

        public static unsafe void Main()
        {
            #region FETCH DATA

            (var result, string source) = Run();
            double[] exchange_rate = (from r in result select r.state.Rate()).ToArray();
            int count = result.Length;

            PlotData dat_rate = new PlotData("Exchange rate", CLR_RATE, exchange_rate, x => !double.IsNaN(x));
            PlotData dat_budg = new PlotData("Bot budget (direct and shares)", CLR_BUDGET, result.Select(x => x.state.AssetCount * x.state.Rate() + x.state.Budget));
            PlotData dat_volm = new PlotData("Share trade volume", CLR_VOLUME, result.Select(x => x.data.Volume)) { intsc = true };
            PlotData dat_asst = new PlotData("Bot trade volume", CLR_VOLUME_BOT, result.Select(x => (double)x.state.AssetCount).deriv((x, y) => Abs(y - x), 0));
            PlotData dat_shbd = new PlotData("Bot budget (shares only)", CLR_BUDGET_SHARES, result.Select(x => x.state.AssetCount * x.state.Rate()));
            PlotData dat_drbd = new PlotData("Bot budget (direct only)", CLR_BUDGET_DIRECT, result.Select(x => x.state.Budget));
            PlotData[] datasets = new[] { dat_rate, dat_budg, dat_shbd, dat_drbd, dat_volm, /*dat_asst*/ };
            
            dat_rate.lst = dat_rate.dataset[dat_rate.dataset.Length - 2];
            
            PointF conv(double x, double y) => new PointF((float)(Min(Max(0, x), 1) * count * FAC_WIDTH + BORDER), (float)(BORDER + (1 - Min(Max(0, y), 1)) * (HEIGHT - 2 * BORDER)));
            Pen cpen(uint c) => new Pen(Color.FromArgb((int)c));

            #endregion
            #region INIT BITMAP + DRAWING

            using (Bitmap bmp = new Bitmap((int)(FAC_WIDTH * count + 2 * BORDER + BORDER_EX_RIGHT), HEIGHT))
            using (Font f_mono = new Font("Consolas", FNT_HEIGHT, GraphicsUnit.Pixel))
            using (Graphics g = Graphics.FromImage(bmp))
            using (Pen p_txt = cpen(CLR_TEXT))
            using (Pen p_txtp = cpen(CLR_TEXT_PALE))
            using (Brush b_txt = new SolidBrush(Color.FromArgb(unchecked((int)CLR_TEXT))))
            {
                g.CompositingQuality = CompositingQuality.HighQuality;
                g.InterpolationMode = InterpolationMode.HighQualityBicubic;
                g.PixelOffsetMode = PixelOffsetMode.HighQuality;
                g.SmoothingMode = SmoothingMode.HighQuality;
                g.Clear(Color.FromArgb(unchecked((int)CLR_BACKGROUND)));

                #endregion
                #region DRAW TEXT + FRAME

                var mbs = new ManagementObjectSearcher("Select ProcessorId From Win32_processor").Get();
                string id = "";

                foreach (ManagementObject mo in mbs)
                    id += mo["ProcessorId"].ToString();

                string lheader = $@"
{count} data points, bot type: {BOT.GetType().FullName}
rate: {dat_rate.fst,8:F2}{CURR} ---> {dat_rate.lst,8:F2}{CURR}  ({dat_rate.lst - dat_rate.fst,8:F2}{CURR} or {(dat_rate.lst - dat_rate.fst) / dat_rate.fst * 100,7:F2} %)
bot:  {dat_budg.fst,8:F2}{CURR} ---> {dat_budg.lst,8:F2}{CURR}  ({dat_budg.lst - dat_budg.fst,8:F2}{CURR} or {(dat_budg.lst - dat_budg.fst) / dat_budg.fst * 100,7:F2} %)
";
                string rheader = $@"
Generated on {DateTime.Now:dddd, yyyy-MMM-dd HH:mm:ss.ffffff} by {Environment.MachineName}:{Environment.UserName}//{Environment.UserName}
{id} ({Environment.OSVersion})
".Trim();
                string footer = $@"
config> |---BUDGET---|--RISK--|--LOOKBACK--|
        |{BUDGET,10:F2} {CURR}|{RISK * 100,6:F2} %|{LOOKBACK,7} days|
source> {source}
";
                
                g.DrawString(footer.Trim(), f_mono, b_txt, 0, HEIGHT - BORDER + 2);
                g.DrawString(lheader.Trim(), f_mono, b_txt, 0, 0);
                g.DrawString(rheader, f_mono, b_txt, BORDER + g.MeasureString(lheader, f_mono).Width, 0);
                g.DrawRectangle(p_txt, BORDER - 1, BORDER - 1, (float)(FAC_WIDTH * count + 2), HEIGHT - 2 * BORDER + 2);

                #endregion
                #region DRAW GRID LINES

                int linecount = (int)((HEIGHT - 2 * BORDER) / (2 * FNT_HEIGHT));

                for (int i = 1; i < linecount; ++i)
                {
                    float y = BORDER + i * (HEIGHT - 2f * BORDER) / linecount;

                    g.DrawLine(p_txtp, BORDER, y, (float)(FAC_WIDTH * count) + BORDER, y);
                }

                for (int i = 1; i < count; i += 5)
                    g.DrawLine(p_txtp, (float)(BORDER + i * FAC_WIDTH), BORDER, (float)(BORDER + i * FAC_WIDTH), HEIGHT - BORDER);

                #endregion
                #region DRAW/PLOT DATA

                for (int i = 1; i < count; ++i)
                {
                    double px = (i - 1) / (double)count;
                    double cx = i / (double)count;
                    double rate = dat_rate[i - 1];
                    string v;

                    foreach (PlotData dat in datasets.Reverse())
                        g.DrawLine(dat, conv(px, dat.Conv(dat[i - 1])), conv(cx, dat.Conv(dat[i])));

                    Pen p = Pens.Transparent;

                    switch (result[i].state.LastAction)
                    {
                        case BotAction.Buy b:
                            p = Pens.LightBlue;
                            v = "+" + b.Item2;
                            break;
                        case BotAction.Sell s:
                            p = Pens.Red;
                            v = "-" + s.Item2;
                            break;
                        default:
                            v = null;
                            break;
                    }

                    const double HGT = .02;

                    g.DrawLine(p, conv(px, dat_rate.Conv(rate) - HGT), conv(px, dat_rate.Conv(rate) + HGT));
                    // g.DrawString(v ?? "", f_mono, p.Brush, conv(px + .002, dat_rate.Conv(rate) + HGT));
                }

                #endregion
                #region DRAW SCALES

                float hoffs = (float)(count * FAC_WIDTH + BORDER);
                float voffs = BORDER;

                foreach (var dat in datasets)
                    hoffs += dat.DrawScale(g, f_mono, hoffs, linecount);

                hoffs += 20;

                g.DrawLine(p_txt, hoffs, BORDER, hoffs, HEIGHT - BORDER);

                hoffs += 20;

                #endregion
                #region DRAW DESCRIPTION

                float desc_wdh = 0;

                foreach (var dat in datasets)
                {
                    g.DrawLine(dat, hoffs, voffs + FNT_HEIGHT / 1.7f, hoffs + 24 , voffs + FNT_HEIGHT / 1.7f);
                    g.DrawString(dat, f_mono, b_txt, hoffs + 25, voffs);

                    desc_wdh = Max(desc_wdh, g.MeasureString(dat, f_mono).Width);

                    voffs += FNT_HEIGHT + 4;
                }

                #endregion
                #region RESIZE BITMAP AND DRAW IL

                int new_wdh = (int)(desc_wdh + hoffs + 40);
#if !USE_CSV
                string IL = string.Join(" ", from b in __fptr.Method.GetMethodBody().GetILAsByteArray() select b.ToString("x2"));
                float cwidth = g.MeasureString(IL, f_mono).Width / IL.Length;
                int cnt = (int)(count * FAC_WIDTH / cwidth);
                string[] IL_lines = IL.SpliceILText(cnt);

                for (int i = 0; i < IL_lines.Length; ++i)
                    IL_lines[i] = (i == 0 ? "IL-bts> " : "        ") + IL_lines[i];
#endif
                using (Bitmap bmp2 = new Bitmap(new_wdh, bmp.Height
#if !USE_CSV
                        + (int)((FNT_HEIGHT + 1) * IL_lines.Length + 5)
#endif
                    ))
                using (Graphics g2 = Graphics.FromImage(bmp2))
                {
                    g2.CompositingQuality = CompositingQuality.HighQuality;
                    g2.InterpolationMode = InterpolationMode.HighQualityBicubic;
                    g2.PixelOffsetMode = PixelOffsetMode.HighQuality;
                    g2.SmoothingMode = SmoothingMode.HighQuality;
                    g2.Clear(Color.FromArgb(unchecked((int)CLR_BACKGROUND)));
                    g2.DrawImageUnscaled(bmp, 0, 0);
#if !USE_CSV
                    g2.DrawString(string.Join("\n", IL_lines), f_mono, b_txt, 0, bmp.Height + 1);
#endif
                    bmp2.Save(TARGET, ImageFormat.Png);
                }

                #endregion
            }

            if (PAUSE && Debugger.IsAttached)
            {
                Console.WriteLine("press any key to exit ...");
                Console.ReadKey(true);
            }
        }

        internal static (double min, double max, double fst, double lst, Func<double, double> conv) GetConv(IEnumerable<double> set, Func<double, bool> cond = null)
        {
            if (cond is null)
                cond = x => true;

            set = set.Where(cond);

            double max = set.Max();
            double min = set.Min();

            return (min, max, set.First(), set.Last(), v => max == min ? 0 : (v - min) / (max - min));
        }

        internal static U[] deriv<T, U>(this IEnumerable<T> tail, Func<T, T, U> f, T head)
        {
            U[] u = new U[tail.Count()];
            T[] t = tail.ToArray();

            for (int i = 0; i < t.Length; ++i)
                u[i] = i > 0 ? f(t[i - 1], t[i]) : f(head, t[0]);

            return u;
        }

        internal static string[] SpliceILText(this string text, int lineLength) =>
            Regex.Matches(text + " ", $@"([0-9a-fA-F]{{2}}\s){{1,{lineLength / 3}}}").Cast<Match>().Select(m => m.Value).ToArray();
    }

    internal static class FSharpExtensions
    {
        public static FSharpOption<T> Create<T>(T value) => new FSharpOption<T>(value);
        public static bool IsSome<T>(this FSharpOption<T> opt) => FSharpOption<T>.get_IsSome(opt);
        public static bool IsNone<T>(this FSharpOption<T> opt) => FSharpOption<T>.get_IsNone(opt);

        public static double Rate(this State s) => s?.ExchangeRate?.Value ?? double.NaN;
    }

    internal struct PlotData
        : IDisposable
    {
        internal Func<double, double> conv;
        public double[] dataset;
        public double max;
        public double min;
        public double fst;
        public double lst;
        public string desc;
        public bool intsc;
        public Brush brush;
        public Pen pen;


        public double this[int i] => dataset[i];

        public PlotData(string desc, uint color, IEnumerable<double> data, Func<double, bool> cond = null)
            : this()
        {
            this.desc = desc;
            dataset = data.ToArray();
            (min, max, fst, lst, conv) = GetConv(data, cond);

            Color c = Color.FromArgb((int)color);

            pen = new Pen(c);
            brush = new SolidBrush(c);
        }

        public double Conv(double inp) => conv(inp);

        public void Dispose()
        {
            brush?.Dispose();
            pen?.Dispose();
        }

        public float DrawScale(Graphics g, Font f, float hoffs, int count)
        {
            float hfac = (HEIGHT - 2f * BORDER) / count;
            float voffs = BORDER;
            float mwdh = 0;
            int w = (int)Log10(Max(Abs(max), Max(Abs(min), 1))) + (intsc ? 1 : 4);

            for (int i = 0; i <= count; ++i)
            {
                double val = min + (count - i) * (max - min) / count;
                string str = (intsc ? "" + (int)val : val.ToString("F2")).PadLeft(w);

                mwdh = Max(mwdh, g.MeasureString(str, f).Width);

                g.DrawLine(pen, hoffs, voffs, hoffs + 10, voffs);
                g.DrawString(str, f, brush, hoffs + 11, voffs - FNT_HEIGHT / 2);

                voffs += hfac;
            }

            return mwdh + 14;
        }
        

        public static implicit operator Pen(PlotData dat) => dat.pen;
        public static implicit operator Brush(PlotData dat) => dat.brush;
        public static implicit operator string(PlotData dat) => dat.desc;
        public static implicit operator Func<double, double>(PlotData dat) => dat.conv;
    }
}
