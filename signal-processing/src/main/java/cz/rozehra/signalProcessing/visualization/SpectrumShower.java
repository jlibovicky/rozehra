package cz.rozehra.signalProcessing.visualization;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.List;

public class SpectrumShower extends XYVisualizer {
    public SpectrumShower(final SpectrogramDrawer drawer, String timeAsString, int timeIndex, double bandWidth, List<Double> values) {
        super("Spectrum at " + timeAsString + " s", bandWidth, "Frequency [Hz]", "Power", values);

        // draw rectangle in original visualizer
        if (drawer != null) drawer.timeRectangle = timeIndex;

        // add the event to remove such rectangle
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
               if (drawer != null) drawer.timeRectangle = -1;
            }
        });

    }
}
