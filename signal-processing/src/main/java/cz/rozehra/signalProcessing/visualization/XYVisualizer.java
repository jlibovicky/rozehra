package cz.rozehra.signalProcessing.visualization;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.util.List;

public class XYVisualizer extends JFrame {
    private static final long serialVersionUID = 1L;

    public XYVisualizer(String chartTitle, double xAxisStep, String xAxisTitle,
                        String yAxisTitle, List<Double> values) {
        super(chartTitle);
        // This will create the dataset
        XYSeries dataset = createDataset(xAxisStep, values);
        // based on the dataset we create the chart
        JFreeChart chart = createChart(dataset, chartTitle, xAxisTitle, yAxisTitle);
        // we put the chart into a panel
        ChartPanel chartPanel = new ChartPanel(chart);
        // default size
        chartPanel.setPreferredSize(new java.awt.Dimension(500, 270));
        // add it to our application
        setContentPane(chartPanel);
        setVisible(true);
    }


    /**
     * Creates a sample dataset
     */
    private  XYSeries createDataset(double xAxisStep, List<Double> values) {
        XYSeries series = new XYSeries("XYGraph");

        for(int i = 0; i < values.size(); ++i) {
            series.add((double)i * xAxisStep, values.get(i));
        }

        return series;
    }


    private JFreeChart createChart(XYSeries series, String title,
                                   String xAxisTitle, String yAxisTitle) {
        XYSeriesCollection dataset = new XYSeriesCollection();
        dataset.addSeries(series);

        JFreeChart chart = ChartFactory.createXYLineChart(
                title,      // chart title
                xAxisTitle,
                yAxisTitle,
                dataset,
                PlotOrientation.VERTICAL, // Plot Orientation
                false, // Show Legend
                true, // Use tooltips
                false);
        return chart;
    }
}
