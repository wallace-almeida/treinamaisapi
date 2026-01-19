package com.treinamaisapi.service.compra.pacote.acessValid;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import com.treinamaisapi.repository.PacoteCompradoRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AcessoPacoteService {

    private final PacoteCompradoRepository repository;

    @Transactional
    public boolean usuarioTemAcesso(Long usuarioId, Long pacoteId) {

        var aprovadas = repository
                .findByUsuarioIdAndPacoteIdAndStatus(usuarioId, pacoteId, StatusCompra.APROVADA);

        boolean temAcesso = false;

        for (PacoteComprado pc : aprovadas) {

            if (pc.isExpirado()) {
                pc.setAtivo(false);
                pc.setStatus(StatusCompra.EXPIRADA);
                repository.save(pc);
            } else {
                // achamos pelo menos uma compra válida
                temAcesso = true;
            }
        }

        return temAcesso;
    }

}

