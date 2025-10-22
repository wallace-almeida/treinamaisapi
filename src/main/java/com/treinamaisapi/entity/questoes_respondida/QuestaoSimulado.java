package com.treinamaisapi.entity.questoes_respondida;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.simulado.Simulado;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "QUESTOES_RESPONDIDAS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuestaoSimulado {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "simulado_id")
    private Simulado simulado;

    @ManyToOne
    @JoinColumn(name = "questao_id")
    private Questao questao;

    private String respostaUsuario= null; // A, B, C, D

    @Column(nullable = true)
    private Boolean correta = null; // true se acertou

    private Double pontuacaoObtida= 0.0; // ex: 1.0 se acertou, 0.0 se errou
}

