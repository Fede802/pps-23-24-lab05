package polyglot.minesweeper;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import polyglot.Pair;


public class GUI extends JFrame {
    
    private static final long serialVersionUID = -6218820567019985015L;
    private final Map<JButton, Pair<Integer,Integer>> buttons = new HashMap<>();
    private final Logics logics;

    public GUI(int size, int minesToPlace) {
        boolean a = polyglot.minesweeper.ClickResult.WIN == polyglot.minesweeper.ClickResult.WIN;
        this.logics = new LogicsImpl(size,minesToPlace);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setSize(100*size, 100*size);

        JPanel panel = new JPanel(new GridLayout(size,size));
        this.getContentPane().add(BorderLayout.CENTER,panel);

        ActionListener onClick = (e)->{
            final JButton bt = (JButton)e.getSource();
            final Pair<Integer,Integer> pos = buttons.get(bt);
            ClickResult clickResult = this.logics.clickCell(pos);
            boolean aMineWasFound = clickResult == ClickResult.LOSE;// call the logic here to tell it that cell at 'pos' has been seleced
            if (aMineWasFound) {
                quitGame();
                JOptionPane.showMessageDialog(this, "You lost!!");
                System.exit(0);
            } else {
                drawBoard();
            }
            boolean isThereVictory = clickResult == ClickResult.WIN; // call the logic here to ask if there is victory
            if (isThereVictory){
                quitGame();
                JOptionPane.showMessageDialog(this, "You won!!");
                System.exit(0);
            }
        };

        MouseInputListener onRightClick = new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                final JButton bt = (JButton)e.getSource();
                if (e.getButton() == MouseEvent.BUTTON3 && bt.isEnabled()){
                    final Pair<Integer,Integer> pos = buttons.get(bt);
                    // call the logic here to put/remove a flag
                    logics.toggleFlag(pos);
                }
                drawBoard();
            }
        };

        for (int i=0; i<size; i++){
            for (int j=0; j<size; j++){
                final JButton jb = new JButton(" ");
                jb.addActionListener(onClick);
                jb.addMouseListener(onRightClick);
                this.buttons.put(jb,new Pair<>(i,j));
                panel.add(jb);
            }
        }
        this.drawBoard();
        this.setVisible(true);
    }

    private void quitGame() {
        this.drawBoard();
        Font largerFont = new Font(Font.SERIF,Font.PLAIN,30);
    	for (var entry: this.buttons.entrySet()) {
            // call the logic here
            // if this button is a mine, draw it "*"
            // disable the button


            if(this.logics.getCellStatus(entry.getValue()).mine()){
                entry.getKey().setFont(largerFont);
                entry.getKey().setText("*");
                entry.getKey().setEnabled(false);
            }
    	}
    }

    private void drawBoard() {
        for (var entry: this.buttons.entrySet()) {
            // call the logic here
            // if this button is a cell with counter, put the number
            // if this button has a flag, put the flag
            GameCellData gameCellData = this.logics.getCellStatus(entry.getValue());
            if(gameCellData.clicked() && !gameCellData.mine()){
                entry.getKey().setText(String.valueOf(gameCellData.minesAround()));
                entry.getKey().setEnabled(false);
            }
            else if(gameCellData.flagged()){
                entry.getKey().setText("F");
            }else{
                entry.getKey().setText("");
            }
    	}
    }
    
}
