using System.Collections.Generic;
using System.Drawing.Drawing2D;
using System.Management;
using System.Drawing;
using System.Linq;
using System;

using static System.Math;


namespace BotVisualizer
{
    public static class Plotter
    {
        public static Bitmap Plot(PlotConfig config, IEnumerable<PlotData> datasets) => Plot(config, datasets?.ToArray() ?? new PlotData[0]);

        public static Bitmap Plot(PlotConfig config, params PlotData[] datasets)
        {
            const int BORDER = 50;
            float charwdh = MeasureString("@", new Font("Consolas", (float)config.FontHeight, GraphicsUnit.Pixel)).Width;

            int count = datasets.Max(set => set.dataset.Length);
            float border_ex_right = datasets.Max(set => set.Description?.Length ?? 0) * charwdh + 50
                                  + (config.JoinScales ? 1 : datasets.Length) * charwdh * 10;

            PointF conv(double x, double y) => new PointF((float)(Min(Max(0, x), 1) * count * config.WidthFactor + BORDER), (float)(BORDER + (1 - Min(Max(0, y), 1)) * (config.Height - 2 * BORDER)));

            if (config.JoinScales)
            {
                double max = datasets.Max(set => set.Max);
                double min = datasets.Min(set => set.Min);

                for (int i = 0; i < datasets.Length; ++i)
                {
                    datasets[i].Max = max;
                    datasets[i].Min = min;
                    datasets[i]._conv = v => max == min ? 0 : (v - min) / (max - min);
                }
            }

            #region INIT BITMAP + DRAWING

            Bitmap bmp = new Bitmap((int)(config.WidthFactor * count + 2 * BORDER + border_ex_right), config.Height);

            using (Font f_mono = new Font("Consolas", (float)config.FontHeight, GraphicsUnit.Pixel))
            using (Graphics g = Graphics.FromImage(bmp))
            using (Brush b_txt = new SolidBrush(config.TextColor))
            using (Pen p_txtp = new Pen(config.PaleTextColor))
            using (Pen p_txt = new Pen(config.TextColor))
            {
                g.CompositingQuality = CompositingQuality.HighQuality;
                g.InterpolationMode = InterpolationMode.HighQualityBicubic;
                g.PixelOffsetMode = PixelOffsetMode.HighQuality;
                g.SmoothingMode = SmoothingMode.HighQuality;
                g.Clear(config.BackgroundColor);

                #endregion
                #region DRAW TEXT + FRAME
                
                string id = string.Concat(new ManagementObjectSearcher("Select ProcessorId From Win32_processor").Get()
                                                                                                                 .Cast<ManagementObject>()
                                                                                                                 .Select(mo => mo["ProcessorId"].ToString()));
                string rheader = $@"
Generated on {DateTime.Now:dddd, yyyy-MMM-dd HH:mm:ss.ffffff} by {Environment.MachineName}:{Environment.UserName}//{Environment.UserName}
{id} ({Environment.OSVersion})
".Trim();
                g.DrawString(rheader.Trim(), f_mono, b_txt, 0, 0);
                g.DrawRectangle(p_txt, BORDER - 1, BORDER - 1, (float)(config.WidthFactor * count + 2), config.Height - 2 * BORDER + 2);

                #endregion
                #region DRAW GRID LINES

                int linecount = (int)((config.Height - 2 * BORDER) / (2 * config.FontHeight));

                for (int i = 1; i < linecount; ++i)
                {
                    float y = BORDER + i * (config.Height - 2f * BORDER) / linecount;

                    g.DrawLine(p_txtp, BORDER, y, (float)(config.WidthFactor * count) + BORDER, y);
                }

                for (int i = 1; i < count; i += 5)
                {
                    float x = (float)(BORDER + i * config.WidthFactor);

                    g.DrawLine(p_txtp, x, BORDER, x, config.Height - BORDER);
                }

                #endregion
                #region DRAW/PLOT DATA

                for (int i = 1; i < count; ++i)
                {
                    double px = (i - 1) / (double)count;
                    double cx = i / (double)count;

                    foreach (PlotData dat in datasets.Reverse())
                    {
                        PointF p1 = conv(px, dat.Conv(dat[i - 1]));
                        PointF p2 = conv(cx, dat.Conv(dat[i]));

                        if (float.IsNaN(p1.Y) || float.IsNaN(p2.Y))
                            continue;

                        g.DrawLine(dat, p1, p2);
                    }
                }

                #endregion
                #region DRAW SCALES

                float hoffs = (float)(count * config.WidthFactor + BORDER);
                float voffs = BORDER;

                if (config.JoinScales)
                {
                    PlotData set = datasets[0];

                    Pen cpy = set.Pen;

                    set.Pen = p_txt;
                    hoffs += set.DrawScale(g, f_mono, hoffs, linecount, config.Height, BORDER, (float)config.FontHeight);
                    set.Pen = cpy;
                }
                else
                    hoffs = datasets.Aggregate(hoffs, (ho, dat) => ho + dat.DrawScale(g, f_mono, ho, linecount, config.Height, BORDER, (float)config.FontHeight));

                hoffs += 20;

                g.DrawLine(p_txt, hoffs, BORDER, hoffs, config.Height - BORDER);

                hoffs += 20;

                #endregion
                #region DRAW DESCRIPTION

                float desc_wdh = 0;

                foreach (PlotData dat in datasets)
                {
                    float y = (float)(voffs + config.FontHeight / 1.7f);

                    g.DrawLine(dat, hoffs, y, hoffs + 24, y);
                    g.DrawString(dat, f_mono, b_txt, hoffs + 25, voffs);

                    desc_wdh = Max(desc_wdh, g.MeasureString(dat, f_mono).Width);

                    voffs += (float)config.FontHeight + 4;
                }

                #endregion

                return bmp;
            }
        }

        public static SizeF MeasureString(string s, Font font)
        {
            using (font)
            using (Bitmap bmp = new Bitmap(1, 1))
            using (Graphics g = Graphics.FromImage(bmp))
                return g.MeasureString(s, font);
        }
    }

    public struct PlotConfig
    {
        public int Height { get; set; }
        public double WidthFactor { get; set; }
        public double FontHeight { get; set; }
        public Color BackgroundColor { get; set; }
        public Color TextColor { get; set; }
        public Color PaleTextColor { get; set; }
        public bool JoinScales { get; set; }


        public PlotConfig(int height, double widthFactor, double fontHeight, bool joinScales, Color backgroundColor, Color textColor, Color paleTextColor): this()
        {
            Height = height;
            WidthFactor = widthFactor;
            FontHeight = fontHeight;
            BackgroundColor = backgroundColor;
            TextColor = textColor;
            PaleTextColor = paleTextColor;
            JoinScales = joinScales;
        }

        public static (PlotConfig config, PlotData[] plots) JoinPlots(PlotConfig config, double y_min, double y_max, IEnumerable<PlotData> plots) =>
            JoinPlots(config, y_min, y_max, plots?.ToArray() ?? new PlotData[0]);

        public static (PlotConfig config, PlotData[]plots) JoinPlots(PlotConfig config, double y_min, double y_max, params PlotData[] plots)
        {
            PlotConfig conf = config;
            int count = plots.Max(plot => plot.dataset?.Length ?? 1);

            conf.JoinScales = true;
            plots = plots.Concat(new[]
            {
                new PlotData(" ", 0u, Enumerable.Repeat(y_min, count)),
                new PlotData(" ", 0u, Enumerable.Repeat(y_max, count)),
            }).ToArray();

            return (conf, plots);
        }
        

        public static PlotConfig Config_DefaultDark { get; } = new PlotConfig(
            1080,
            1,
            14,
            false,
            Color.FromArgb(unchecked((int)0xff222222u)),
            Color.FromArgb(unchecked((int)0xffeeeeeeu)),
            Color.FromArgb(unchecked((int)0xff444444u))
        );
    }

    public struct PlotData
        : IDisposable
    {
        internal Func<double, double> _conv;

        internal double[] dataset { get; }
        public double Max { get; internal set; }
        public double Min { get; internal set;  }
        public double First { get; }
        public double Last { get; }
        public string Description { get; }
        public Brush Brush { get; }
        public Pen Pen { get; internal set; }

        public bool intsc;


        public double this[int i] => dataset[i];

        public PlotData(string desc, uint color, IEnumerable<double> data, Func<double, bool> cond = null)
            : this()
        {
            Description = desc;
            dataset = data.ToArray();
            (Min, Max, First, Last, _conv) = GetConv(data, cond);

            Color c = Color.FromArgb((int)color);

            Pen = new Pen(c);
            Brush = new SolidBrush(c);
        }

        public double Conv(double inp) => _conv(inp);

        public void Dispose()
        {
            Brush?.Dispose();
            Pen?.Dispose();
        }

        internal float DrawScale(Graphics g, Font f, float hoffs, int count, int height, int border, float font_height)
        {
            float hfac = (height - 2f * border) / count;
            float voffs = border;
            float mwdh = 0;
            int w = (int)Log10(Max(Abs(Max), Max(Abs(Min), 1))) + (intsc ? 1 : 4);

            for (int i = 0; i <= count; ++i)
            {
                double val = Min + (count - i) * (Max - Min) / count;
                string str = (intsc ? "" + (int)val : val.ToString("F2")).PadLeft(w);

                mwdh = Max(mwdh, g.MeasureString(str, f).Width);

                g.DrawLine(Pen, hoffs, voffs, hoffs + 10, voffs);
                g.DrawString(str, f, Brush, hoffs + 11, voffs - font_height / 2);

                voffs += hfac;
            }

            return mwdh + 14;
        }

        private static (double min, double max, double fst, double lst, Func<double, double> conv) GetConv(IEnumerable<double> set, Func<double, bool> cond = null)
        {
            if (cond is null)
                cond = x => true;

            set = set.Where(cond);

            double max = set.Max();
            double min = set.Min();

            if (double.IsNaN(max))
                max = min;

            if (double.IsNaN(min))
                min = 0;

            return (min, max, set.First(), set.Last(), v => max == min ? 0 : (v - min) / (max - min));
        }


        public static implicit operator Pen(PlotData dat) => dat.Pen;
        public static implicit operator Brush(PlotData dat) => dat.Brush;
        public static implicit operator string(PlotData dat) => dat.Description;
        public static implicit operator Func<double, double>(PlotData dat) => dat._conv;
    }
}
