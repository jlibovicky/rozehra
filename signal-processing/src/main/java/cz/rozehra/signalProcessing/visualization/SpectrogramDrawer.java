package cz.rozehra.signalProcessing.visualization;

import cz.rozehra.signalProcessing.Spectrogram;
import cz.rozehra.signalProcessing.Spectrum;
import cz.rozehra.signalProcessing.partialtracking.Track;
import cz.rozehra.signalProcessing.tempo.Tempo;
import cz.rozehra.signalProcessing.trackSelection.Hypothesis;
import cz.rozehra.signalProcessing.trackSelection.Note;
import scala.collection.JavaConverters;
import scala.collection.Seq;

import javax.swing.*;
import java.awt.*;
import java.util.List;

class SpectrogramDrawer extends JPanel {
    public int timeRectangle = -1;
    private Spectrogram spectrogram;
    private List<Double> onsetTimes;
    private Tempo tempo;

    private List<Track> partialTracks;
    private double partialTrackSamplingRate;

    private List<Seq<Double>> fundamentals;
    private double fundamentalsSamplingRate;

    private Hypothesis hypothesis;

    public SpectrogramDrawer() {
        setLayout(null);
    }

    public void addSpectrogram(Spectrogram spectrogram) {
        this.spectrogram = spectrogram;
        setPreferredSize(new Dimension(Visualizer.horizontalScale * spectrogram.spectra().size(),
                spectrogram.bandsCount() + 30));

    }

    public void addOnsetTimes(List<Double> onsetTimes) {
        this.onsetTimes = onsetTimes;
    }

    public void addTempo(Tempo tempo) {
        this.tempo = tempo;
    }

    public void addFundamentalsCandidates(List<Seq<Double>> fundamentals, double fundamentalsSamplingRate) {
        this.fundamentals = fundamentals;
        this.fundamentalsSamplingRate = fundamentalsSamplingRate;
    }

    public void addPartialTracks(List<Track> partialTracks, double partialTrackSamplingRate) {
        this.partialTracks = partialTracks;
        this.partialTrackSamplingRate = partialTrackSamplingRate;
    }

    public void addHypothesis(Hypothesis hypothesis) {
        this.hypothesis = hypothesis;
    }


    public void paintComponent(Graphics g) {
        if(spectrogram == null) { return; }

        //int spectumCount = 0;
        java.util.List<Spectrum> spectra = scala.collection.JavaConversions.asJavaList(spectrogram.spectra().toSeq());

        double maximum = 0.0;
        for (Spectrum s: spectra) {
            for(int i = 0; i < s.amplitudes().size(); i++) {
                if ((Double)(s.amplitudes().apply(i)) > maximum) maximum = (Double)(s.amplitudes().apply(i));
            }
        }

        int startIndex = (int) (this.getVisibleRect().getX() / Visualizer.horizontalScale);
        int endIndex = (int) ((this.getVisibleRect().getX() +
                this.getVisibleRect().getWidth()) / Visualizer.horizontalScale);

        for (int j = startIndex; j < endIndex; j++) {
            Spectrum s = spectra.get(j);
        //for (Spectrum s : spectra) {
            for(int i = 0; i < s.amplitudes().size(); i++) {
                int colorIntensity = (int)Math.round(255.0 - (255.0 * (Double)(s.amplitudes().apply(i)) / maximum));

                g.setColor(new Color(colorIntensity, colorIntensity, colorIntensity));
                g.fillRect(Visualizer.horizontalScale * j, s.amplitudes().size() - i, 2, 1);
            }
        }

        // plot the time scale
        g.setColor(Color.BLACK);
        int tenthOfSecondsCount = (int)Math.floor(10 * spectra.size() / spectrogram.spectrumRate());
        for (int i = 0; i <= tenthOfSecondsCount; i++) {
            int horizontalPos = Visualizer.horizontalShift + i * Visualizer.horizontalScale * (int)(spectrogram.spectrumRate()) / 10;
            if (i % 10 == 0 ) {
                g.drawLine(horizontalPos, spectrogram.bandsCount(),
                        horizontalPos, spectrogram.bandsCount() + 10);
                g.drawString((i / 10) + "s", horizontalPos - 4, spectrogram.bandsCount() + 23);
            }
            else if (i % 5 == 0) {
                g.drawLine(horizontalPos, spectrogram.bandsCount(),
                        horizontalPos, spectrogram.bandsCount() + 6);
            }
            else {
                g.drawLine(horizontalPos, spectrogram.bandsCount(),
                        horizontalPos, spectrogram.bandsCount() + 4);
            }

        }

        setSize(new Dimension(Visualizer.horizontalShift + Visualizer.horizontalScale * spectra.size(), spectrogram.bandsCount() + 30));

        if (onsetTimes != null) {  paintOnsets(g); }
        if (tempo != null) { paintTempo(g); }
        if (fundamentals != null) { paintFundamentals(g); }
        if (partialTracks != null) { paintTracks(g); }
        if (timeRectangle != -1) { paintTimeRectangle(g); }
        if (hypothesis != null) { paintHypothesis(g); }
    }

    private void paintOnsets(Graphics g) {
        g.setColor(Color.BLUE);

        for(Double onsetTime: onsetTimes) {
            int horizontalPos = (int) (Visualizer.horizontalScale * onsetTime * spectrogram.spectrumRate());
            g.drawLine(horizontalPos, 0, horizontalPos, spectrogram.bandsCount());
        }
    }

    private void paintTempo(Graphics g) {
        g.setColor(Color.LIGHT_GRAY);

        Graphics2D g2d = (Graphics2D) g;
        Stroke originalStroke = g2d.getStroke();

        float[] dash = { 4f, 0f, 2f };
        BasicStroke dottedStyle = new BasicStroke(1, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_ROUND, 1.0f, dash, 2f );

        // compute the number to visualize
        int beatsCount = (int)Math.floor(spectrogram.spectrumDuration() * spectrogram.spectra().size() * tempo.tempo());

        for (int i = 0; i < beatsCount; i++) {
            double beatTime = (double)i / tempo.tempo() - tempo.shift();
            if (beatTime < 0) { continue; }
            int horizontalPos =  (int) (Visualizer.horizontalScale * beatTime * spectrogram.spectrumRate());
            g.drawLine(horizontalPos, 0, horizontalPos, spectrogram.bandsCount());
        }

        g2d.setStroke(originalStroke);

    }

    private void paintFundamentals(Graphics g) {
        Graphics2D g2d = (Graphics2D) g;

        g2d.setColor(new Color(255, 0, 0));
        double horizontalRatio = spectrogram.spectrumRate() / fundamentalsSamplingRate;

        int spectrumCount = 0;
        for (Seq<Double> candidates : fundamentals) {
            int horizontalPos = (int) (Visualizer.horizontalScale * spectrumCount * horizontalRatio);
            for (Double frequency : JavaConverters.asJavaIterableConverter(candidates).asJava()) {
                g.drawRect(horizontalPos, spectrogram.bandsCount() -
                        (int)(frequency * spectrogram.bandsCount() / spectrogram.maxFrequency()), 1, 1);
            }

            spectrumCount++;
        }
    }

    private void paintTracks(Graphics g) {
        g.setColor(new Color(150, 0, 0));
        double horizontalRatio = spectrogram.spectrumRate() / partialTrackSamplingRate;

        for(Track t: partialTracks) {
            int verticalPos =  spectrogram.bandsCount() - (int)(t.averageFrequency() * spectrogram.bandsCount() / spectrogram.maxFrequency());
            g.drawRect((int) (Visualizer.horizontalScale * t.start() * horizontalRatio),
                    verticalPos - 1, (int) (Visualizer.horizontalScale * horizontalRatio * (t.end() - t.start())), 3);
        }
    }

    private void paintTimeRectangle(Graphics g) {
        g.setColor(new Color(0, 0, 160));
        if (timeRectangle != -1) {
            g.drawRect(timeRectangle * Visualizer.horizontalScale - 1, 0,
                    Visualizer.horizontalScale + 1, spectrogram.bandsCount());
        }
    }

    public void paintHypothesis(Graphics g) {
        g.setColor(new Color(200, 100, 0));
        for (int i = 0; i < hypothesis.notes().size(); i++) {
            Note note = hypothesis.notes().apply(i);
            int verticalPos =  spectrogram.bandsCount() - (int)(note.pitchAsFrequency() * spectrogram.bandsCount()
                    / spectrogram.maxFrequency());
            g.fillRect((int)(note.start() * spectrogram.spectrumRate()) - 1, verticalPos,
                    (int)(note.duration() * spectrogram.spectrumRate()), 4);
        }
    }


}