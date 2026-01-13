package com.treinamaisapi.service.compra.pacote.acessValid;

import com.treinamaisapi.entity.enums.pacotes.StatusCompra;
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

        return repository
                .findByUsuarioIdAndPacoteIdAndStatus(usuarioId, pacoteId, StatusCompra.APROVADA)
                .map(pc -> {
                    if (pc.isExpirado()) {
                        pc.setAtivo(false);
                        pc.setStatus(StatusCompra.EXPIRADA);
                        repository.save(pc);
                        return false;
                    }

                    return true;
                })
                .orElse(false);
    }
}

