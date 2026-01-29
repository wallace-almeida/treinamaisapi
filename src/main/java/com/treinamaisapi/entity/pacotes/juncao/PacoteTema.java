package com.treinamaisapi.entity.pacotes.juncao;

import com.treinamaisapi.entity.pacotes.Pacote;
import com.treinamaisapi.entity.tema.Tema;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "pacote_temas")
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
public class PacoteTema {

    @EmbeddedId
    private PacoteTemaId id = new PacoteTemaId();

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @MapsId("pacoteId")
    @JoinColumn(name = "pacote_id", nullable = false)
    private Pacote pacote;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @MapsId("temaId")
    @JoinColumn(name = "tema_id", nullable = false)
    private Tema tema;

    public PacoteTema(Pacote pacote, Tema tema) {
        this.pacote = pacote;
        this.tema = tema;
    }
}