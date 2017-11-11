// #define USE_CSV

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.FSharp.Core;

using TradingBot;

using static TradingBot.Simulator;
using static TradingBot.Bot;
using static System.Math;

namespace BotVisualizer
{
    public partial class Form1
        : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

#pragma warning disable CS0168, CS0219
        private void button1_Click(object sender, EventArgs e)
        {
            double st = 30;

            double sim(double x)
            {
                if (x < 400 || x > 450)
                    st += .2;
                else
                    st -= 1;

                return st;

                // return Sin(x) / 2 + 100 + Cos(4 * x) / 4 + Sin(3 * Sin(x)) / 8 - 10 * Atan(x / 20 - 30);
            }
            IBot bot = new Bot_01();
            ulong lookback = 3;
            double budget = 1000;
            double max = 2000;
            double risk = .2;

            // https://www.google.com/finance/historical?output=csv&q= ...
#if USE_CSV
            Tuple<ulong, State>[] result = SimulateCSV(bot, "data/sinewave.csv", risk, budget, lookback);
#else
            Tuple<ulong, State>[] result = SimulateFake(bot, FakeFunc(sim, max), risk, budget, lookback);
#endif

        }
#pragma warning restore CS0168, CS0219

        private static FSharpFunc<ulong, FSharpOption<double>> FakeFunc(Func<double, double> f, double max) =>
            create(new AbstractRate(new Converter<double, double>(f), max)) as FSharpFunc<ulong, FSharpOption<double>>;
    }
}
