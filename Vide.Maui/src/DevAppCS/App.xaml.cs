namespace DevAppCS
{
    public partial class App : Application
    {
        public App()
        {
            InitializeComponent();

            //MainPage = new AppShell();
            //MainPage = DevApp.Demos.noVideDemo;

            var host = DevApp.Demos.start(DevApp.Demos.simpleVideDemo());
            MainPage = new ContentPage { Content = host };
        }
    }
}