package cz.rozehra.signalProcessing.visualization;

import cz.rozehra.signalProcessing.Spectrogram;
import cz.rozehra.signalProcessing.Spectrum;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.text.DecimalFormat;

public class SpectrumOnClickListener implements MouseListener {
    private static DecimalFormat df = new DecimalFormat("#.##");
    private Spectrogram spectrogram;

    public  SpectrumOnClickListener(Spectrogram spectrogram) {
        this.spectrogram = spectrogram;
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        SpectrogramDrawer drawer = (SpectrogramDrawer)e.getSource();
        e.getPoint();
        int timeIndex = e.getPoint().x / Visualizer.horizontalScale;
        double time = (double)timeIndex / spectrogram.spectrumRate();

        Spectrum spectrum = (Spectrum)(spectrogram.spectra().apply(timeIndex));
        new SpectrumShower(drawer, df.format(time), timeIndex, spectrogram.bandWidth(),
                scala.collection.JavaConversions.asJavaList(spectrum.amplitudes().toSeq()));

    }

    @Override
    public void mousePressed(MouseEvent e) {}

    @Override
    public void mouseReleased(MouseEvent e) { }

    @Override
    public void mouseEntered(MouseEvent e) { }

    @Override
    public void mouseExited(MouseEvent e) { }
}
