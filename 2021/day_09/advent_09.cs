using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Advent2021
{
    class Day9
    {
        static void Main(string[] args)
        {
            int n = 100;
            int[,] field = ReadInputs(n);

            int risksSum = 0;
            List<Tuple<int, int>> lowPoints = new List<Tuple<int, int>>();

            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    bool low = true;
                    low &= (i == 0)     || field[i,j] < field[i-1,j];
                    low &= (i == n - 1) || field[i,j] < field[i+1,j];
                    low &= (j == 0)     || field[i,j] < field[i,j-1];
                    low &= (j == n - 1) || field[i,j] < field[i,j+1];
                    risksSum += (low ? 1 : 0) * (field[i,j] + 1);
                    if (low) { lowPoints.Add(new Tuple<int, int>(i, j)); }
                }
            }

            var walker = new BasinWalker(field);
            List<int> max = new List<int>();
            foreach (var lowPoint in lowPoints)
            {
                int size = walker.getSizeOfBasin(lowPoint);
                if (max.Count < 3 || size > max.Min()) max.Add(size);
                if (max.Count > 3) max.Remove(max.Min());
            }

            Console.WriteLine($"Risk levels sum is {risksSum}");
            Console.WriteLine($"Product of the three lasrgest basins is {max.Aggregate(1, (acc, it) => acc * it)}");
        }

        private static int[,] ReadInputs(int n)
        {
            int[,] field = new int[n, n];
            for (int i = 0; i < n; i++)
            {
                char[] elements = Console.ReadLine()!.ToCharArray();
                for (int j = 0; j < n; j++) field[i,j] = int.Parse(elements[j].ToString());
            }
            return field;
        }

        class BasinWalker
        {
            private int[,] field;
            public BasinWalker(int [,] field)
            {
                this.field = field;
            }

            public int getSizeOfBasin(Tuple<int, int> start) { return Walk(new HashSet<Tuple<int, int>>(), start); }

            private int Walk(HashSet<Tuple<int, int>> visited, Tuple<int, int> pos)
            {
                if (visited.Contains(pos)) return visited.Count;
                var (i, j) = pos;
                if (i < 0 || i >= field.GetLength(0) || j < 0 || j >= field.GetLength(1) || field[i,j] > 8) return visited.Count;
                visited.Add(pos);
                Walk(visited, new Tuple<int, int>(i + 1, j));
                Walk(visited, new Tuple<int, int>(i - 1, j));
                Walk(visited, new Tuple<int, int>(i, j + 1));
                Walk(visited, new Tuple<int, int>(i, j - 1));
                return visited.Count;
            }
        }
    }
}
