package cz.rozehra.signalProcessing.visualization;

import cz.rozehra.signalProcessing.Spectrogram;
import cz.rozehra.signalProcessing.partialtracking.Track;
import cz.rozehra.signalProcessing.tempo.Tempo;
import cz.rozehra.signalProcessing.trackSelection.Hypothesis;
import scala.collection.Seq;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class Visualizer extends JFrame {
    public static final int horizontalShift = 0;
    public static final int horizontalScale = 2;

    private JFrame window;
    private JPanel mainPanel;
    private JScrollPane scrollPane;
    private SpectrogramDrawer drawer;
    private FrequencyScaleDrawer frequencyScale;
    private JLabel timeLabel;
    private JLabel freqLabel;

    private Spectrogram<?> spectrogram;

    public Visualizer() {
        window = new JFrame();
        window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        window.setBounds(30, 30, 1050, 600);


        mainPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));

        frequencyScale = new FrequencyScaleDrawer();
        mainPanel.add(frequencyScale);

        drawer = new SpectrogramDrawer();
        scrollPane = new JScrollPane(drawer);
        scrollPane.setPreferredSize(new Dimension(800, 565));
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        scrollPane.setLocation(40, 0);
        mainPanel.add(scrollPane);

        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(null);
        rightPanel.setPreferredSize(new Dimension(150, 567));
        mainPanel.add(rightPanel);

        JLabel timeTitle = new JLabel();
        timeTitle.setBounds(10, 400, 100, 20);
        timeTitle.setText("time");
        rightPanel.add(timeTitle);

        timeLabel = new JLabel();
        timeLabel.setBounds(25, 420, 120, 20);
        timeLabel.setText("??? s");
        timeLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        timeLabel.setFont(timeLabel.getFont().deriveFont(18.0f));
        rightPanel.add(timeLabel);

        JLabel freqTitle = new JLabel();
        freqTitle.setBounds(10, 450, 100, 20);
        freqTitle.setText("frequency");
        rightPanel.add(freqTitle);

        freqLabel = new JLabel();
        freqLabel.setBounds(25, 470, 120, 20);
        freqLabel.setHorizontalAlignment(SwingConstants.RIGHT);
        freqLabel.setText("??? Hz");
        freqLabel.setFont(freqLabel.getFont().deriveFont(18.0f));
        rightPanel.add(freqLabel);

        window.getContentPane().add(mainPanel);
        window.setVisible(true);
    }

    public void drawSpectrum(Spectrogram<?> spectrogram) {
        drawer.addSpectrogram(spectrogram);
        frequencyScale.addSpectrogram(spectrogram);
        frequencyScale.setPreferredSize(new Dimension(
                (int)frequencyScale.getPreferredSize().getWidth(),
                (int)scrollPane.getPreferredSize().getHeight()));
        drawer.addMouseListener(
                new SpectrumOnClickListener(spectrogram));
        drawer.addMouseMotionListener(
                new SpectrumOnMouseListener(freqLabel, timeLabel,
                        spectrogram.bandWidth(), spectrogram.spectrumRate(), spectrogram.bandsCount()));
    }

    public void drawNoteOnsets(List<Double> noteOnsets) {
        drawer.addOnsetTimes(noteOnsets);
    }

    public void drawTempo(Tempo tempo) {
        drawer.addTempo(tempo);
    }

    public void drawFundamentals(List<Seq<Double>> fundamentals, double fundamentalsSamplingRate) {
        drawer.addFundamentalsCandidates(fundamentals, fundamentalsSamplingRate);
    }

    public void drawPartialTracks(List<Track> partialTracks, double trackingSamplingRate) {
        drawer.addPartialTracks(partialTracks, trackingSamplingRate);
    }

    public void drawHypothesis(Hypothesis hypothesis) {
        drawer.addHypothesis(hypothesis);
    }
}
