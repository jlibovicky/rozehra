package cz.rozehra.signalProcessing.visualization;


import javax.swing.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.text.DecimalFormat;

public class SpectrumOnMouseListener implements MouseMotionListener {
    private JLabel frequencyLabel;
    private JLabel timeLabel;
    private double bandWidth;
    private double sampleRate;
    private int bandCount;

    private static DecimalFormat df = new DecimalFormat("#.##");

    public SpectrumOnMouseListener(JLabel frequencyLabel, JLabel timeLabel, double bandWidth, double sampleRate, int bandCount) {
        this.frequencyLabel = frequencyLabel;
        this.timeLabel = timeLabel;
        this.bandWidth = bandWidth;
        this.sampleRate = sampleRate;
        this.bandCount = bandCount;

    }

    @Override
    public void mouseDragged(MouseEvent e)  { }

    @Override
    public void mouseMoved(MouseEvent e) {
        int x = e.getPoint().x;
        int y = e.getPoint().y;

        double time = (double)x / (double)Visualizer.horizontalScale / sampleRate;
        double frequency = (bandCount - (double)y - Visualizer.topFreqStart) * bandWidth;

        timeLabel.setText(df.format(time) + " s");
        if (frequency > 0) { frequencyLabel.setText(df.format(frequency) + " Hz"); }
    }
}
