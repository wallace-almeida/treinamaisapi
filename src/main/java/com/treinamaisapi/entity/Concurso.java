package com.treinamaisapi.entity;

import com.treinamaisapi.entity.pacotes.Pacote;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "concursos")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Concurso {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String nome; // Ex: "EAER 2025"

    @Column(columnDefinition = "TEXT")
    private String descricao; // Ex: "Concurso da Escola de Especialistas da Aeronáutica"

    private LocalDate dataProva;

    @OneToMany(mappedBy = "concurso", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Pacote> pacotes = new ArrayList<>();

}
