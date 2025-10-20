package com.treinamaisapi.entity.questoes_respondida;

import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.simulado.Simulado;
import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "QUESTOES_RESPONDIDAS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuestaoRespondida {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String respostaUsuario;

    private boolean correta;

    private Double pontuacaoObtida;

    private LocalDateTime dataResposta;

    @ManyToOne
    @JoinColumn(name = "questao_id")
    private Questao questao;

    @ManyToOne
    @JoinColumn(name = "simulado_id")
    private Simulado simulado;
}

