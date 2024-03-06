using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using MvcMovie.Models;
using MvcMovie.Utils;

namespace MvcMovie.Data
{
    public class MvcMovieContext : DbContext
    {
        public MvcMovieContext (DbContextOptions<MvcMovieContext> options)
            : base(options)
        {}

        public DbSet<MvcMovie.Models.Movie> Movie { get; set; } = default!;

        public string CustomMethod(string someValue)
        {
            MyUtils u = new MyUtils(Movie);
            // DEEP: 
            var result = print(someValue);
            return "Hello world " + result;
        }
    }
}
