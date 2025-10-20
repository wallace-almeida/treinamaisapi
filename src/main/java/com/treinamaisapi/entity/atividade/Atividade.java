package com.treinamaisapi.entity.atividade;




import com.treinamaisapi.entity.baralho.Baralho;
import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.enums.TipoAtividade;
import com.treinamaisapi.entity.questoes.Questao;
import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.tema.Tema;
import com.treinamaisapi.entity.usuarios.Usuario;
import jakarta.persistence.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "atividade")
public class Atividade {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String titulo; // ex: "Simulado Matemática - 20 questões"

    @Enumerated(EnumType.STRING)
    @Column(name = "tipo_atividade", nullable = false)
    private TipoAtividade tipoAtividade;

    @Enumerated(EnumType.STRING)
    @Column(name = "nivel_dificuldade")
    private NivelDificuldade nivelDificuldade;

    private Integer pontuacaoBase; // pontos possíveis para a atividade

    private Integer duracaoMinutos; // tempo estimado (opcional)

    private LocalDateTime dataCriacao = LocalDateTime.now();

    // referências opcionais para o recurso concreto
    @ManyToOne
    @JoinColumn(name = "simulado_id")
    private Simulado simulado; // se for tipo SIMULADO

    @ManyToOne
    @JoinColumn(name = "baralho_id")
    private Baralho baralho; // se for tipo FLASHCARD

    @ManyToOne
    @JoinColumn(name = "questao_id")
    private Questao questao; // se for tipo QUESTAO (questão isolada)

    @ManyToOne
    @JoinColumn(name = "usuario_id", nullable = false)
    private Usuario usuario; // quem criou (ou quem realizou, dependendo do caso)

    @ManyToOne
    @JoinColumn(name = "tema_id")
    private Tema tema;

    // Status se necessário (CRIA, ATIVO, CONCLUIDO) - opcional
    private String status;

    // getters e setters...
}