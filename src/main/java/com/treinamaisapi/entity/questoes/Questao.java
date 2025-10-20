package com.treinamaisapi.entity.questoes;

import com.treinamaisapi.entity.enums.NivelDificuldade;
import com.treinamaisapi.entity.tema.Tema;
import jakarta.persistence.*;
import lombok.*;


@Entity
@Table(name = "QUESTOES")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Questao {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, columnDefinition = "TEXT")
    private String enunciado;

    @Column(columnDefinition = "JSON")
    private String alternativas; // armazenadas em formato JSON

    private String respostaCorreta;

    @Enumerated(EnumType.STRING)
    private NivelDificuldade dificuldade;

    @ManyToOne
    @JoinColumn(name = "tema_id")
    private Tema tema;
}
