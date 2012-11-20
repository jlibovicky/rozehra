package cz.rozehra.signalProcessing.visualization;

import cz.rozehra.signalProcessing.Spectrogram;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class Visualizer extends JFrame {
    private JFrame window;

    public Visualizer() {
        window = new JFrame();
        window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        window.setBounds(30, 30, 300, 300);
        window.setVisible(true);
    }

    public void drawSpectrum(Spectrogram<?> spectrogram) {
        window.getContentPane().add(new SpectrogramDrawer(spectrogram));


        // plot the frequency scale
        /*int thousandsOfHertzs = (int)(Math.floor(spectrogram.maxFrequency() / 1000));
        double hundredHertzHeight = 100 * spectrogram.bandsCount() / spectrogram.maxFrequency();
        for (int i = 0; i <= thousandsOfHertzs; i++) {
            int verticalPos = (int)(hundredHertzHeight * i);

            JLabel label = new JLabel();
            label.setText((i * 1000) + "Hz");
            label.setBounds(0, verticalPos - 5, SpectrogramDrawer.horizontalShift - 10, verticalPos + 5) ;
            window.getContentPane().add(label);
        } */

    }
}

class SpectrogramDrawer extends JComponent {
    private Spectrogram spectrogram;

    public static final int horizontalShift = 20;
    public static final int horizontalScale = 2;

    public SpectrogramDrawer(Spectrogram spectrogram) {
        this.spectrogram = spectrogram;
    }


    public void paint(Graphics g) {
        int spectumCount = 0;
        List<Spectrum> spectra = scala.collection.JavaConversions.asJavaList(spectrogram.spectra().toSeq());

        double maximum = 0.0;
        for (Spectrum s: spectra) {
            for(int i = 0; i < s.amplitudes().size(); i++) {
                if ((Double)(s.amplitudes().apply(i)) > maximum) maximum = (Double)(s.amplitudes().apply(i));
            }
        }

        for (Spectrum s: spectra) {
            spectumCount++;
            for(int i = 0; i < s.amplitudes().size(); i++) {
                int colorIntensity = (int)Math.round(255.0 - (255.0 * (Double)(s.amplitudes().apply(i)) / maximum));

                g.setColor(new Color(colorIntensity, colorIntensity, colorIntensity));
                g.fillRect(horizontalShift + horizontalScale * spectumCount, s.amplitudes().size() - i, 2, 1);
            }
        }

        // plot the time scale
        g.setColor(Color.BLACK);
        int tenthOfSecondsCount = (int)Math.floor(10 * spectumCount / spectrogram.spectrumRate());
        for (int i = 0; i <= tenthOfSecondsCount; i++) {
            int horizontalPos = horizontalShift + i * horizontalScale * (int)(spectrogram.spectrumRate()) / 10;
            if (i % 10 == 0 ) {
                g.drawLine(horizontalPos, spectrogram.bandsCount(),
                        horizontalPos, spectrogram.bandsCount() + 10);
                g.drawString((i / 10) + "s", horizontalPos - 4, spectrogram.bandsCount() + 12);
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
    }
}
