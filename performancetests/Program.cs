using System;
using ModelUnitTests;

namespace performancetests
{
    class Program
    {
        static void Main(string[] args)
        {
            FSharpTryToImproveScheduleTests obj = new FSharpTryToImproveScheduleTests();
            obj.TryToImproveSchedule00052();
        }
    }
}
